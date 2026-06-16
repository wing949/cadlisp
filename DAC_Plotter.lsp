;;; ==========================================================================
;;; LISP IN HÀNG LOẠT TRONG MODEL VÀ LAYOUT (BATCH PLOT) CHO AUTOCAD / ENJICAD / ZWCAD
;;; Lệnh vẽ đường chéo khung in: VDC
;;; Lệnh thực hiện in hàng loạt: DP (Hiện Popup chọn Máy in, Khổ giấy, Nét in)
;;; Bản quyền thuộc về MrChuong thuộc Công ty CP Quản lý dự án DAC
;;; Tự động xoay khổ giấy (Landscape/Portrait) theo tỷ lệ đường chéo/khung bao
;;; Tự động sắp xếp thứ tự in từ Trái sang Phải, Từ Trên xuống Dưới
;;; ==========================================================================

(vl-load-com)
(if (not (boundp '*inhl-paper-cache*))
  (setq *inhl-paper-cache* nil)
)
(if (not (boundp '*inhl-fast-plot*))
  (setq *inhl-fast-plot* t)
)
(if (not (boundp '*inhl-manual-drawings*))
  (setq *inhl-manual-drawings* nil)
)
(if (not (boundp '*inhl-enjicad-extra-pdfs*))
  (setq *inhl-enjicad-extra-pdfs* nil)
)
(if (not (boundp '*inhl-old-modemacro*))
  (setq *inhl-old-modemacro* nil)
)

;;; ==========================================================================
;;; HÀM HỖ TRỢ LẤY THÔNG TIN HỆ THỐNG CAD
;;; ==========================================================================

;;; Hàm kiểm tra hoặc tạo Layer đặc biệt chuyên dụng cho đường chéo (Không in ra giấy)
(defun inhl:get-or-create-layer ( / acad-obj doc layers layer-obj)
  (setq acad-obj (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad-obj)
        layers (vla-get-layers doc))
  (setq layer-obj (vl-catch-all-apply 'vla-item (list layers "DAC_Plotter")))
  (if (vl-catch-all-error-p layer-obj)
    (progn
      (setq layer-obj (vla-add layers "DAC_Plotter"))
      (vla-put-color layer-obj 4) ; Màu Cyan (Màu xanh nước biển) dễ nhìn
      (vla-put-plottable layer-obj :vlax-false) ; Thiết lập KHÔNG IN (Non-plottable)
      (princ "\n-> Đã tạo mới Layer chuyên dụng: DAC_Plotter (Không in ra khi xuất PDF/in ấn)")
    )
  )
  layer-obj
)

;;; Lấy danh sách máy in/plotter có sẵn trong CAD hiện hành
(defun inhl:get-plotters (layout / result value names)
  (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list layout))
  (setq result (vl-catch-all-apply 'vla-GetPlotDeviceNames (list layout)))
  (if (vl-catch-all-error-p result)
    nil
    (progn
      (setq value (vl-catch-all-apply 'vlax-variant-value (list result)))
      (if (vl-catch-all-error-p value)
        nil
        (progn
          (setq names (vl-catch-all-apply 'vlax-safearray->list (list value)))
          (if (vl-catch-all-error-p names) nil names)
        )
      )
    )
  )
)

;;; Lấy danh sách bảng nét in (.ctb/.stb) từ CAD hiện hành
(defun inhl:add-unique-ci (value values / found item)
  (if (and value (/= value ""))
    (progn
      (foreach item values
        (if (= (strcase item) (strcase value)) (setq found T))
      )
      (if found values (append values (list value)))
    )
    values
  )
)

(defun inhl:split-path-list (text / pos result item)
  (while (and text (/= text ""))
    (setq pos (vl-string-search ";" text))
    (if pos
      (setq item (substr text 1 pos)
            text (substr text (+ pos 2)))
      (setq item text
            text nil)
    )
    (if (/= item "") (setq result (append result (list item))))
  )
  result
)

(defun inhl:add-style-dir (dir dirs)
  (if (and dir (/= dir "") (vl-file-directory-p dir))
    (inhl:add-unique-ci dir dirs)
    dirs
  )
)

(defun inhl:style-dir-from-file (file dirs / found)
  (setq found (findfile file))
  (if found
    (inhl:add-style-dir (vl-filename-directory found) dirs)
    dirs
  )
)

(defun inhl:get-preference-style-path ( / acad prefs files value)
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (not (vl-catch-all-error-p acad))
    (progn
      (setq prefs (vl-catch-all-apply 'vlax-get-property (list acad 'Preferences)))
      (if (not (vl-catch-all-error-p prefs))
        (progn
          (setq files (vl-catch-all-apply 'vlax-get-property (list prefs 'Files)))
          (if (not (vl-catch-all-error-p files))
            (progn
              (setq value (vl-catch-all-apply 'vlax-get-property (list files 'PrinterStyleSheetPath)))
              (if (vl-catch-all-error-p value)
                nil
                value
              )
            )
          )
        )
      )
    )
  )
)

(defun inhl:collect-style-dirs (root depth / result child full files item)
  (if (and root (> depth 0) (vl-file-directory-p root))
    (progn
      (setq files (append (vl-directory-files root "*.ctb" 1) (vl-directory-files root "*.stb" 1)))
      (if files (setq result (inhl:add-unique-ci root result)))
      (foreach child (vl-directory-files root nil -1)
        (if (and (/= child ".") (/= child ".."))
          (progn
            (setq full (strcat (inhl:folder-with-slash root) child))
            (foreach item (inhl:collect-style-dirs full (1- depth))
              (setq result (inhl:add-unique-ci item result))
            )
          )
        )
      )
    )
  )
  result
)

(defun inhl:get-plot-style-dirs ( / dirs value root roam appdata)
  (foreach value (inhl:split-path-list (inhl:get-preference-style-path))
    (setq dirs (inhl:add-style-dir value dirs))
  )
  (foreach value (list (getenv "PrinterStyleSheetDir") (getenv "PrinterStyleSheetPath"))
    (foreach root (inhl:split-path-list value)
      (setq dirs (inhl:add-style-dir root dirs))
    )
  )
  (setq roam (vl-catch-all-apply 'getvar (list "ROAMABLEROOTPREFIX")))
  (if (and (not (vl-catch-all-error-p roam)) roam)
    (setq dirs (inhl:add-style-dir (strcat (inhl:folder-with-slash roam) "Plotters\\Plot Styles") dirs))
  )
  (foreach value '("monochrome.ctb" "acad.ctb" "grayscale.ctb" "monochrome.stb")
    (setq dirs (inhl:style-dir-from-file value dirs))
  )
  (setq appdata (getenv "APPDATA"))
  (if appdata
    (foreach root
      (list
        (strcat appdata "\\EnjiCAD")
        (strcat appdata "\\ZWSOFT")
        (strcat appdata "\\Autodesk")
      )
      (foreach value (inhl:collect-style-dirs root 4)
        (setq dirs (inhl:add-unique-ci value dirs))
      )
    )
  )
  dirs
)

(defun inhl:get-ctbs ( / dirs result name dir)
  (foreach dir (inhl:get-plot-style-dirs)
    (foreach name (append (vl-directory-files dir "*.ctb" 1) (vl-directory-files dir "*.stb" 1))
      (setq result (inhl:add-unique-ci name result))
    )
  )
  (vl-sort result '(lambda (a b) (< (strcase a) (strcase b))))
)

;;; Lấy đường dẫn file cấu hình lưu lần cuối
(defun inhl:get-config-path ( / path)
  (setq path (findfile "DAC_Plotter.lsp"))
  (if path
    (strcat (vl-filename-directory path) "\\DAC_Plotter_config.txt")
    (strcat (getvar "dwgprefix") "DAC_Plotter_config.txt")
  )
)

;;; Hàm nạp cấu hình đã lưu lần trước
(defun inhl:clean-text (s)
  (if s
    (vl-string-trim " \t\r\n" s)
    ""
  )
)

(defun inhl:get-kcs-config-path ( / path)
  (setq path (findfile "DAC_Plotter.lsp"))
  (if path
    (strcat (vl-filename-directory path) "\\KCS_Plotter_20190103\\Config.txt")
    nil
  )
)

(defun inhl:load-kcs-settings ( / cfg-path f line p-name p-size c-style)
  (setq cfg-path (inhl:get-kcs-config-path))
  (if (and cfg-path (findfile cfg-path))
    (progn
      (setq f (open cfg-path "r"))
      (if f
        (progn
          (setq line (read-line f))
          (close f)
          (if line
            (progn
              (setq p-name (inhl:clean-text (substr line 1 100)))
              (setq p-size (inhl:clean-text (substr line 101 10)))
              (setq c-style (inhl:clean-text (substr line 111 50)))
              (if (and (/= p-name "") (/= p-size ""))
                (list p-name p-size c-style)
                nil
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun inhl:load-settings ( / cfg-path f p-name p-size c-style)
  (setq cfg-path (inhl:get-config-path))
  (if (and cfg-path (findfile cfg-path))
    (progn
      (setq f (open cfg-path "r"))
      (if f
        (progn
          (setq p-name (read-line f)
                p-size (read-line f)
                c-style (read-line f))
          (close f)
          (list p-name p-size c-style)
        )
        nil
      )
    )
    (inhl:load-kcs-settings)
  )
)

;;; Hàm ghi cấu hình hiện tại xuống file cấu hình
(defun inhl:save-settings (p-name p-size c-style / cfg-path f)
  (setq cfg-path (inhl:get-config-path))
  (if cfg-path
    (progn
      (setq f (open cfg-path "w"))
      (if f
        (progn
          (write-line (if p-name p-name "") f)
          (write-line (if p-size p-size "") f)
          (write-line (if c-style c-style "") f)
          (close f)
          t
        )
        nil
      )
    )
    nil
  )
)

;;; ==========================================================================
;;; LỆNH 1: VDC - VẼ ĐƯỜNG CHÉO XÁC ĐỊNH KHUNG IN
;;; Tự động vẽ trên Layer "DAC_Plotter" (Không in ra giấy)
;;; ==========================================================================
(defun c:VDC ( / old-lay old-cmdecho old-osm pt1 pt2)
  ;; Thiết lập hệ thống
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osm (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  
  ;; Đảm bảo có Layer chuyên dụng
  (inhl:get-or-create-layer)
  
  ;; Lưu layer hiện tại và chuyển sang layer vẽ đường chéo
  (setq old-lay (getvar "CLAYER"))
  (setvar "CLAYER" "DAC_Plotter")
  
  (princ "\n=== LỆNH VẼ ĐƯỜNG CHÉO KHUNG IN (LAYER: DAC_Plotter) ===")
  (princ "\n-> Gợi ý: Hãy bật chế độ bắt điểm (OSNAP) để kích chọn chính xác góc khung bản vẽ.")
  
  ;; Vòng lặp cho phép kích vẽ liên tiếp nhiều khung bản vẽ
  (while (and (setq pt1 (getpoint "\nChọn điểm góc thứ nhất (hoặc nhấn chuột phải/Enter để dừng): "))
              (setq pt2 (getpoint pt1 "\nChọn điểm góc đối diện: ")))
    (entmake (list
               '(0 . "LINE")
               '(8 . "DAC_Plotter") ; Gán trực tiếp vào layer chuyên dụng
               (cons 10 pt1)
               (cons 11 pt2)
             ))
    (princ "\n-> Đã vẽ thành công đường chéo cho 1 khung bản vẽ.")
  )
  
  ;; Khôi phục trạng thái ban đầu
  (setvar "CLAYER" old-lay)
  (setvar "CMDECHO" old-cmdecho)
  (princ "\n-> Đã dừng lệnh vẽ đường chéo (VDC).")
  (princ)
)

;;; ==========================================================================
;;; HÀM TRỢ GIÚP CHO LỆNH IN
;;; ==========================================================================

;;; Hàm định dạng số thứ tự (ví dụ: 1 -> "01", 12 -> "12")
(defun inhl:pad-str (num len / s)
  (setq s (itoa num))
  (while (< (strlen s) len)
    (setq s (strcat "0" s))
  )
  s
)

;;; Hàm sắp xếp danh sách khung bản vẽ từ Trái -> Phải, Trên -> Dưới
(defun inhl:filename-name (path / base ext)
  (setq base (vl-filename-base path))
  (setq ext (vl-filename-extension path))
  (strcat (if base base "") (if ext ext ""))
)

(defun inhl:quote (s)
  (strcat "\"" s "\"")
)

(defun inhl:folder-with-slash (folder)
  (if (and folder (> (strlen folder) 0) (/= (substr folder (strlen folder) 1) "\\"))
    (strcat folder "\\")
    folder
  )
)

(defun inhl:file-exists-p (path)
  (and path (vl-file-size path))
)

(defun inhl:safe-getvar (name / result)
  (setq result (vl-catch-all-apply 'getvar (list name)))
  (if (vl-catch-all-error-p result) nil result)
)

(defun inhl:safe-setvar (name value / result)
  (setq result (vl-catch-all-apply 'setvar (list name value)))
  (not (vl-catch-all-error-p result))
)

(defun inhl:get-com-property (obj prop)
  (vl-catch-all-apply 'vlax-get-property (list obj prop))
)

(defun inhl:put-com-property (obj prop value)
  (vl-catch-all-apply 'vlax-put-property (list obj prop value))
)

(defun inhl:get-cad-id ( / acad name caption product result)
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (not (vl-catch-all-error-p acad))
    (progn
      (setq result (vl-catch-all-apply 'vlax-get-property (list acad 'Name)))
      (if (not (vl-catch-all-error-p result)) (setq name result))
      (setq result (vl-catch-all-apply 'vlax-get-property (list acad 'Caption)))
      (if (not (vl-catch-all-error-p result)) (setq caption result))
    )
  )
  (setq result (vl-catch-all-apply 'vlax-product-key '()))
  (if (not (vl-catch-all-error-p result)) (setq product result))
  (strcase (strcat (if name name "") " " (if caption caption "") " " (if product product "")))
)

(defun inhl:enjicad-p ( / cad-id)
  (setq cad-id (inhl:get-cad-id))
  (or
    (wcmatch cad-id "*ENJI*")
    (wcmatch cad-id "*GCAD*")
  )
)

(defun inhl:ensure-folder (folder)
  (if (and folder (not (vl-file-directory-p folder)))
    (vl-mkdir folder)
  )
  (and folder (vl-file-directory-p folder))
)

(defun inhl:decimal-char-p (ch)
  (or
    (wcmatch ch "#")
    (= ch ".")
    (= ch ",")
  )
)

(defun inhl:normalize-decimal-token (token / idx ch result)
  (setq idx 1
        result "")
  (while (<= idx (strlen token))
    (setq ch (substr token idx 1))
    (setq result (strcat result (if (= ch ",") "." ch)))
    (setq idx (1+ idx))
  )
  result
)

(defun inhl:extract-numbers (text / idx ch token numbers)
  (setq idx 1
        token ""
        numbers nil)
  (if text
    (progn
      (while (<= idx (strlen text))
        (setq ch (substr text idx 1))
        (if (inhl:decimal-char-p ch)
          (setq token (strcat token ch))
          (progn
            (if (> (strlen token) 0)
              (setq numbers (append numbers (list (atof (inhl:normalize-decimal-token token)))))
            )
            (setq token "")
          )
        )
        (setq idx (1+ idx))
      )
      (if (> (strlen token) 0)
        (setq numbers (append numbers (list (atof (inhl:normalize-decimal-token token)))))
      )
    )
  )
  numbers
)

(defun inhl:paper-size-from-name (canonical localized / numbers reversed w h)
  (setq numbers (inhl:extract-numbers canonical))
  (if (< (length numbers) 2)
    (setq numbers (inhl:extract-numbers localized))
  )
  (if (>= (length numbers) 2)
    (progn
      (setq reversed (reverse numbers)
            h (car reversed)
            w (cadr reversed))
      (list w h)
    )
    nil
  )
)

(defun inhl:paper-landscape-p (canonical localized / size)
  (setq size (inhl:paper-size-from-name canonical localized))
  (if size
    (> (car size) (cadr size))
    nil
  )
)

(defun inhl:same-paper-size-p (size1 size2 / w1 h1 w2 h2)
  (if (and size1 size2)
    (progn
      (setq w1 (max (car size1) (cadr size1))
            h1 (min (car size1) (cadr size1))
            w2 (max (car size2) (cadr size2))
            h2 (min (car size2) (cadr size2)))
      (and (< (abs (- w1 w2)) 0.5) (< (abs (- h1 h2)) 0.5))
    )
    nil
  )
)

(defun inhl:paper-full-bleed-p (canonical localized / text)
  (setq text (strcase (strcat (if canonical canonical "") " " (if localized localized ""))))
  (wcmatch text "*FULL*BLEED*")
)

(defun inhl:paper-for-orientation (canonicals localizeds base-canonical base-localized want-landscape / base-size base-landscape base-full-bleed idx candidate localized candidate-size candidate-full-bleed fallback found)
  (setq base-size (inhl:paper-size-from-name base-canonical base-localized)
        base-landscape (if base-size (> (car base-size) (cadr base-size)) nil)
        base-full-bleed (inhl:paper-full-bleed-p base-canonical base-localized)
        idx 0)
  (cond
    ((and base-size (= base-landscape want-landscape))
      (list base-canonical base-localized)
    )
    (base-size
      (while (and (< idx (length canonicals)) (null found))
        (setq candidate (nth idx canonicals)
              localized (if localizeds (nth idx localizeds) candidate)
              candidate-size (inhl:paper-size-from-name candidate localized)
              candidate-full-bleed (inhl:paper-full-bleed-p candidate localized))
        (if (and
              candidate-size
              (inhl:same-paper-size-p base-size candidate-size)
              (= (> (car candidate-size) (cadr candidate-size)) want-landscape)
            )
          (progn
            (if (null fallback)
              (setq fallback (list candidate localized))
            )
            (if (= candidate-full-bleed base-full-bleed)
              (setq found (list candidate localized))
            )
          )
        )
        (setq idx (1+ idx))
      )
      (if found
        found
        (if fallback fallback (list base-canonical base-localized))
      )
    )
    (t
      (list base-canonical base-localized)
    )
  )
)

(defun inhl:safe-index (idx lst / max-idx)
  (if (or (null lst) (not (numberp idx)))
    0
    (progn
      (setq max-idx (1- (length lst)))
      (cond
        ((< idx 0) 0)
        ((> idx max-idx) max-idx)
        (t idx)
      )
    )
  )
)

(defun inhl:get-tile-int (key default / value)
  (setq value (get_tile key))
  (if (and value (/= value ""))
    (atoi value)
    default
  )
)

(defun inhl:get-canonical-media-names (layout / result)
  (setq result (vl-catch-all-apply 'vla-GetCanonicalMediaNames (list layout)))
  (if (vl-catch-all-error-p result)
    nil
    (vlax-safearray->list (vlax-variant-value result))
  )
)

(defun inhl:get-papers-for-printer (layout printer / old-config canonicals localizeds filtered-papers)
  (setq old-config (vla-get-configname layout))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-configname (list layout printer))))
    (progn
      (vla-RefreshPlotDeviceInfo layout)
      (setq canonicals (inhl:get-canonical-media-names layout))
      (if canonicals
        (progn
          (setq localizeds
            (mapcar
              '(lambda (name / loc)
                 (setq loc (vl-catch-all-apply 'vla-GetLocaleMediaName (list layout name)))
                 (if (vl-catch-all-error-p loc) name loc)
               )
              canonicals
            )
          )
          (setq filtered-papers (inhl:filter-a0-a4-papers canonicals localizeds))
        )
      )
      (vl-catch-all-apply 'vla-put-configname (list layout old-config))
    )
  )
  filtered-papers
)

(defun inhl:get-papers-cached (layout printer / cached papers)
  (setq cached (assoc printer *inhl-paper-cache*))
  (if cached
    (cdr cached)
    (progn
      (setq papers (inhl:get-papers-for-printer layout printer))
      (if papers
        (setq *inhl-paper-cache* (cons (cons printer papers) *inhl-paper-cache*))
      )
      papers
    )
  )
)

(defun inhl:pdf-file-plotter-p (name / upper)
  (if name
    (progn
      (setq upper (strcase name))
      (or
        (wcmatch upper "*DWG TO PDF*")
        (wcmatch upper "*AUTOCAD PDF*")
      )
    )
    nil
  )
)

(defun inhl:hidden-plotter-p (name / upper)
  (setq upper (strcase (if name name "")))
  (wcmatch upper "DAC_NOVIEWER_*")
)

(defun inhl:pc3-with-extension (name / ext)
  (setq ext (if name (vl-filename-extension name) nil))
  (if (and name ext (= (strcase ext) ".PC3"))
    name
    (if name (strcat name ".pc3") nil)
  )
)

(defun inhl:pc3-without-extension (name / ext)
  (setq ext (if name (vl-filename-extension name) nil))
  (if (and name ext (= (strcase ext) ".PC3"))
    (substr name 1 (- (strlen name) 4))
    name
  )
)

(defun inhl:add-unique (value lst)
  (if (and value (not (member value lst)))
    (append lst (list value))
    lst
  )
)

(defun inhl:find-plotter-name (candidate all-plotters / found item)
  (if candidate
    (foreach item all-plotters
      (if (and (null found) (= (strcase item) (strcase candidate)))
        (setq found item)
      )
    )
  )
  found
)

(defun inhl:pc3-file-exists-p (name / pc3 dir)
  (setq pc3 (inhl:pc3-with-extension name))
  (or
    (and name (findfile name))
    (and pc3 (findfile pc3))
    (and
      pc3
      (setq dir (getenv "PrinterConfigDir"))
      (inhl:file-exists-p (strcat (inhl:folder-with-slash dir) pc3))
    )
  )
)

(defun inhl:no-viewer-plotter-name (name all-plotters / base candidate candidates found)
  (if (inhl:pdf-file-plotter-p name)
    (progn
      (setq base (inhl:pc3-without-extension name)
            candidates nil)
      (foreach candidate
        (list
          (strcat "DAC_NoViewer_" name)
          (inhl:pc3-with-extension (strcat "DAC_NoViewer_" name))
          (strcat "DAC_NoViewer_" base)
          (inhl:pc3-with-extension (strcat "DAC_NoViewer_" base))
        )
        (setq candidates (inhl:add-unique candidate candidates))
      )
      (foreach candidate candidates
        (if (and (null found) (inhl:find-plotter-name candidate all-plotters))
          (setq found (inhl:find-plotter-name candidate all-plotters))
        )
      )
      (foreach candidate candidates
        (if (and (null found) (inhl:pc3-file-exists-p candidate))
          (setq found (inhl:pc3-with-extension candidate))
        )
      )
      (if found found name)
    )
    name
  )
)

(defun inhl:find-qpdf ( / candidates found path)
  (setq candidates
    (list
      "C:\\Program Files\\PDF24\\qpdf\\bin\\qpdf.exe"
      "C:\\Program Files\\PDF24\\qpdf\\qpdf.exe"
      "C:\\Program Files\\qpdf\\bin\\qpdf.exe"
      "C:\\Program Files (x86)\\qpdf\\bin\\qpdf.exe"
    )
  )
  (foreach path candidates
    (if (and (null found) (inhl:file-exists-p path))
      (setq found path)
    )
  )
  found
)

(defun inhl:run-hidden-wait (cmd / shell result ok)
  (setq shell (vlax-create-object "WScript.Shell"))
  (setq result (vl-catch-all-apply 'vlax-invoke-method (list shell 'Run cmd 0 :vlax-true)))
  (if shell (vlax-release-object shell))
  (setq ok (and (not (vl-catch-all-error-p result)) (= result 0)))
  ok
)

(defun inhl:run-qpdf-command (cmd work-dir step / shell cmd-file err-file out-file f result ok)
  (setq cmd-file (strcat work-dir "_qpdf_run_" (inhl:pad-str step 4) ".cmd")
        err-file (strcat work-dir "_qpdf_error.txt")
        out-file (strcat work-dir "_qpdf_output.txt"))
  (if (inhl:file-exists-p cmd-file) (vl-file-delete cmd-file))
  (if (inhl:file-exists-p err-file) (vl-file-delete err-file))
  (if (inhl:file-exists-p out-file) (vl-file-delete out-file))
  (setq f (open cmd-file "w"))
  (if f
    (progn
      (write-line "@echo off" f)
      (write-line (strcat cmd " > " (inhl:quote out-file) " 2> " (inhl:quote err-file)) f)
      (write-line "exit /b %ERRORLEVEL%" f)
      (close f)
      (setq shell (vlax-create-object "WScript.Shell"))
      (setq result (vl-catch-all-apply 'vlax-invoke-method (list shell 'Run (strcat "cmd.exe /d /c " (inhl:quote cmd-file)) 0 :vlax-true)))
      (if shell (vlax-release-object shell))
      (setq ok (and (not (vl-catch-all-error-p result)) (or (= result 0) (= result 3))))
      (if (inhl:file-exists-p cmd-file) (vl-file-delete cmd-file))
      (if ok
        (progn
          (if (inhl:file-exists-p err-file) (vl-file-delete err-file))
          (if (inhl:file-exists-p out-file) (vl-file-delete out-file))
        )
      )
      ok
    )
    nil
  )
)

(defun inhl:qpdf-merge-command (qpdf input-files output-path / cmd)
  (setq cmd (strcat (inhl:quote qpdf) " --warning-exit-0 --empty --pages"))
  (foreach pdf input-files
    (setq cmd (strcat cmd " " (inhl:quote (strcat "--file=" pdf))))
  )
  (strcat cmd " -- " (inhl:quote output-path))
)

(defun inhl:combine-pdfs-qpdf (pdf-files output-path work-dir / qpdf remaining accumulator next-pdf out-pdf temp-results cmd ok idx)
  (setq qpdf (inhl:find-qpdf))
  (if (and qpdf pdf-files output-path work-dir)
    (progn
      (if (inhl:file-exists-p output-path) (vl-file-delete output-path))
      (if (= (length pdf-files) 1)
        (progn
          (setq cmd (inhl:qpdf-merge-command qpdf pdf-files output-path))
          (and (inhl:run-qpdf-command cmd work-dir 1) (inhl:file-exists-p output-path))
        )
        (progn
          (setq accumulator (car pdf-files)
                remaining (cdr pdf-files)
                temp-results nil
                ok t
                idx 1)
          (while (and ok remaining)
            (setq next-pdf (car remaining))
            (if (cdr remaining)
              (progn
                (setq out-pdf (strcat work-dir "_qpdf_merge_" (inhl:pad-str idx 4) ".pdf"))
                (setq temp-results (cons out-pdf temp-results))
              )
              (setq out-pdf output-path)
            )
            (if (inhl:file-exists-p out-pdf) (vl-file-delete out-pdf))
            (setq cmd (inhl:qpdf-merge-command qpdf (list accumulator next-pdf) out-pdf))
            (setq ok (and (inhl:run-qpdf-command cmd work-dir idx) (inhl:file-exists-p out-pdf)))
            (if ok
              (setq accumulator out-pdf
                    remaining (cdr remaining)
                    idx (1+ idx))
            )
          )
          (inhl:delete-files temp-results)
          (and ok (inhl:file-exists-p output-path))
        )
      )
    )
    nil
  )
)

(defun inhl:delete-files (files)
  (foreach path files
    (if (inhl:file-exists-p path)
      (vl-file-delete path)
    )
  )
)

(defun inhl:remove-path-ci (path files)
  (vl-remove-if
    '(lambda (item) (= (strcase item) (strcase path)))
    files
  )
)

(defun inhl:delete-temp-files (folder / files name)
  (if (and folder (vl-file-directory-p folder))
    (progn
      (setq files (vl-directory-files folder nil 1))
      (foreach name files
        (vl-file-delete (strcat folder name))
      )
    )
  )
)

(defun inhl:delay-ms (ms)
  (vl-catch-all-apply 'vl-cmdf (list "_.DELAY" ms))
)

(defun inhl:wait-file (path tries / count)
  (setq count 0)
  (while (and (not (inhl:file-exists-p path)) (< count tries))
    (inhl:delay-ms 250)
    (setq count (1+ count))
  )
  (inhl:file-exists-p path)
)

(defun inhl:file-time-key (path / tm)
  (setq tm (vl-catch-all-apply 'vl-file-systime (list path)))
  (if (and tm (not (vl-catch-all-error-p tm)))
    (apply 'strcat (mapcar '(lambda (n) (inhl:pad-str n 2)) tm))
    ""
  )
)

(defun inhl:pdf-snapshot (folders / result folder name path)
  (foreach folder folders
    (if (and folder (vl-file-directory-p folder))
      (foreach name (vl-directory-files folder "*.pdf" 1)
        (setq path (strcat (inhl:folder-with-slash folder) name))
        (setq result (cons (cons (strcase path) (inhl:file-time-key path)) result))
      )
    )
  )
  result
)

(defun inhl:changed-pdf-after-snapshot (folders snapshot / best best-time folder name path key old)
  (foreach folder folders
    (if (and folder (vl-file-directory-p folder))
      (foreach name (vl-directory-files folder "*.pdf" 1)
        (setq path (strcat (inhl:folder-with-slash folder) name)
              key (inhl:file-time-key path)
              old (assoc (strcase path) snapshot))
        (if (and (/= key "") (or (null old) (/= key (cdr old))) (or (null best-time) (> key best-time)))
          (setq best path
                best-time key)
        )
      )
    )
  )
  best
)

(defun inhl:changed-pdfs-after-snapshot (folders snapshot / result folder name path key old)
  (foreach folder folders
    (if (and folder (vl-file-directory-p folder))
      (foreach name (vl-directory-files folder "*.pdf" 1)
        (setq path (strcat (inhl:folder-with-slash folder) name)
              key (inhl:file-time-key path)
              old (assoc (strcase path) snapshot))
        (if (and (/= key "") (or (null old) (/= key (cdr old))))
          (setq result (inhl:add-unique path result))
        )
      )
    )
  )
  result
)

(defun inhl:copy-pdf-to-temp (source target / result)
  (if (and source target (inhl:file-exists-p source))
    (progn
      (if (inhl:file-exists-p target) (vl-file-delete target))
      (setq result (vl-catch-all-apply 'vl-file-copy (list source target)))
      (and (not (vl-catch-all-error-p result)) (inhl:file-exists-p target))
    )
    nil
  )
)

(defun inhl:path-in-folder-p (path folder / full-path full-folder)
  (if (and path folder)
    (progn
      (setq full-path (strcase (inhl:folder-with-slash (vl-filename-directory path)))
            full-folder (strcase (inhl:folder-with-slash folder)))
      (= full-path full-folder)
    )
    nil
  )
)

(defun inhl:record-enjicad-extra-pdfs (pdfs temp-folder / path)
  (foreach path pdfs
    (if (and path (not (inhl:path-in-folder-p path temp-folder)))
      (setq *inhl-enjicad-extra-pdfs* (inhl:add-unique path *inhl-enjicad-extra-pdfs*))
    )
  )
  *inhl-enjicad-extra-pdfs*
)

(defun inhl:delete-enjicad-extra-pdfs-now (folders snapshot temp-folder / count changed before after)
  (setq count 0
        before nil
        after nil)
  (while (< count 4)
    (setq changed (inhl:changed-pdfs-after-snapshot folders snapshot))
    (setq before (length *inhl-enjicad-extra-pdfs*))
    (inhl:record-enjicad-extra-pdfs changed temp-folder)
    (setq after (length *inhl-enjicad-extra-pdfs*))
    (if (/= before after)
      (progn
        (inhl:delete-files *inhl-enjicad-extra-pdfs*)
        (setq *inhl-enjicad-extra-pdfs* nil)
      )
    )
    (inhl:delay-ms 100)
    (setq count (1+ count))
  )
)

(defun inhl:delete-folder (folder / fso result)
  (if (and folder (vl-file-directory-p folder))
    (progn
      (setq fso (vlax-create-object "Scripting.FileSystemObject"))
      (setq result (vl-catch-all-apply 'vlax-invoke-method (list fso 'DeleteFolder folder :vlax-true)))
      (if fso (vlax-release-object fso))
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

(defun inhl:delete-temp-folder (folder / tries)
  (setq tries 0)
  (while (and folder (vl-file-directory-p folder) (< tries 20))
    (inhl:delete-temp-files folder)
    (inhl:delete-folder folder)
    (if (vl-file-directory-p folder)
      (inhl:delay-ms 250)
    )
    (setq tries (1+ tries))
  )
  (not (vl-file-directory-p folder))
)

(defun inhl:cleanup-temp-pdfs (pdf-files folder)
  (inhl:delete-files pdf-files)
  (inhl:delete-temp-folder folder)
)

(defun inhl:collect-temp-pdfs (folder / result name)
  (if (and folder (vl-file-directory-p folder))
    (progn
      (foreach name (vl-directory-files folder "*.pdf" 1)
        (setq result (cons (strcat (inhl:folder-with-slash folder) name) result))
      )
      (vl-sort result '(lambda (a b) (< (strcase a) (strcase b))))
    )
    nil
  )
)

(defun inhl:open-file (path)
  (if (inhl:file-exists-p path)
    (startapp "explorer.exe" (inhl:quote path))
  )
)

(defun inhl:model-layout-p (layout-name)
  (= (strcase (if layout-name layout-name "")) "MODEL")
)

(defun inhl:show-progress (msg / old-nomutt)
  (setq old-nomutt (inhl:safe-getvar "NOMUTT"))
  (if (null *inhl-old-modemacro*)
    (setq *inhl-old-modemacro* (inhl:safe-getvar "MODEMACRO"))
  )
  (inhl:safe-setvar "NOMUTT" 0)
  (inhl:safe-setvar "MODEMACRO" msg)
  (grtext -1 msg)
  (princ (strcat "\n" msg))
  (princ)
  (if (inhl:enjicad-p)
    (inhl:delay-ms 1)
  )
  (if old-nomutt
    (inhl:safe-setvar "NOMUTT" old-nomutt)
  )
)

(defun inhl:clear-progress ()
  (if *inhl-old-modemacro*
    (progn
      (inhl:safe-setvar "MODEMACRO" *inhl-old-modemacro*)
      (setq *inhl-old-modemacro* nil)
    )
  )
  (grtext -1 "")
  (princ)
)

(defun inhl:pdf-printer-p (name / upper)
  (if name
    (progn
      (setq upper (strcase name))
      (or
        (inhl:pdf-file-plotter-p name)
        (wcmatch upper "*PDF24*")
        (wcmatch upper "*PDFFACTORY*")
        (wcmatch upper "*PDF FACTORY*")
        (wcmatch upper "*MICROSOFT PRINT TO PDF*")
      )
    )
    nil
  )
)

(defun inhl:preferred-pdf-plotter (plotters / found)
  (foreach name plotters
    (if (and (null found) (inhl:pdf-file-plotter-p name))
      (setq found name)
    )
  )
  (if (null found)
    (foreach name plotters
      (if (and (null found) (inhl:pdf-printer-p name))
        (setq found name)
      )
    )
  )
  found
)

(defun inhl:a0-a4-paper-p (canonical localized / text)
  (setq text (strcase (strcat (if canonical canonical "") " " (if localized localized ""))))
  (or
    (wcmatch text "*A0*")
    (wcmatch text "*A1*")
    (wcmatch text "*A2*")
    (wcmatch text "*A3*")
    (wcmatch text "*A4*")
  )
)

(defun inhl:filter-a0-a4-papers (canonicals localizeds / idx filtered-c filtered-l)
  (setq idx 0
        filtered-c nil
        filtered-l nil)
  (foreach canonical canonicals
    (if (inhl:a0-a4-paper-p canonical (nth idx localizeds))
      (progn
        (setq filtered-c (cons canonical filtered-c))
        (setq filtered-l (cons (nth idx localizeds) filtered-l))
      )
    )
    (setq idx (1+ idx))
  )
  (if filtered-c
    (list (reverse filtered-c) (reverse filtered-l))
    (list canonicals localizeds)
  )
)

(defun inhl:find-paper-index (paper canonicals localizeds / target idx found names name)
  (if paper
    (progn
      (setq found (vl-position paper canonicals))
      (if (null found)
        (setq found (vl-position paper localizeds))
      )
      (if (null found)
        (progn
          (setq target (strcase paper))
          (setq idx 0)
          (setq names (mapcar 'strcase (append canonicals localizeds)))
          (foreach name names
            (if (and (null found) (wcmatch name (strcat "*" target "*")))
              (setq found (rem idx (length canonicals)))
            )
            (setq idx (1+ idx))
          )
        )
      )
      found
    )
    nil
  )
)

(defun inhl:point2d (pt / arr)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill arr (list (car pt) (cadr pt)))
  arr
)

(defun inhl:get-layout-names (doc / result item)
  (setq result nil)
  (vlax-for item (vla-get-layouts doc)
    (setq result (cons (list (vla-get-taborder item) (vla-get-name item)) result))
  )
  (mapcar 'cadr (vl-sort result '(lambda (a b) (< (car a) (car b)))))
)

(defun inhl:get-layout-by-name (doc name / result)
  (setq result (vl-catch-all-apply 'vla-item (list (vla-get-layouts doc) name)))
  (if (vl-catch-all-error-p result) nil result)
)

(defun inhl:activate-layout (layout-name / result)
  (if (= (strcase layout-name) "MODEL")
    (progn
      (setq result (vl-catch-all-apply 'setvar (list "TILEMODE" 1)))
      (not (vl-catch-all-error-p result))
    )
    (progn
      (vl-catch-all-apply 'setvar (list "TILEMODE" 0))
      (setq result (vl-catch-all-apply 'setvar (list "CTAB" layout-name)))
      (if (not (vl-catch-all-error-p result))
        (vl-catch-all-apply 'vl-cmdf (list "_.PSPACE"))
      )
      (not (vl-catch-all-error-p result))
    )
  )
)

(defun inhl:entity-layout-name (dxf / name)
  (setq name (cdr (assoc 410 dxf)))
  (if (and name (/= name ""))
    name
    "Model"
  )
)

(defun inhl:list-contains-ci (value values / found item)
  (setq found nil)
  (foreach item values
    (if (= (strcase value) (strcase item))
      (setq found t)
    )
  )
  found
)

(defun inhl:position-ci (value values / idx found item)
  (setq idx 0
        found nil)
  (foreach item values
    (if (and (null found) (= (strcase value) (strcase item)))
      (setq found idx)
    )
    (setq idx (1+ idx))
  )
  found
)

(defun inhl:add-unique-ci (value values)
  (if (and value (/= value "") (not (inhl:list-contains-ci value values)))
    (cons value values)
    values
  )
)

(defun inhl:get-layer-names (doc / result layer)
  (setq result nil)
  (vlax-for layer (vla-get-layers doc)
    (setq result (cons (vla-get-name layer) result))
  )
  (vl-sort result '<)
)

(defun inhl:block-effective-name (obj / result)
  (setq result (vl-catch-all-apply 'vla-get-effectivename (list obj)))
  (if (vl-catch-all-error-p result)
    (vla-get-name obj)
    result
  )
)

(defun inhl:get-block-names ( / tbl name result)
  (setq result nil)
  (setq tbl (tblnext "BLOCK" t))
  (while tbl
    (setq name (cdr (assoc 2 tbl)))
    ;; Loại bỏ các block ẩn, layout hoặc anonymous bắt đầu bằng dấu *
    (if (not (wcmatch name "`**"))
      (setq result (cons name result))
    )
    (setq tbl (tblnext "BLOCK"))
  )
  (if result
    (vl-sort result '<)
    (list "<Không có block>")
  )
)

(defun inhl:ssget-title-blocks (block-name / ss result idx ent obj name)
  (setq ss (ssget "X" '((0 . "INSERT"))))
  (setq result (ssadd))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (setq obj (vlax-ename->vla-object ent))
        (setq name (inhl:block-effective-name obj))
        (if (= (strcase name) (strcase block-name))
          (ssadd ent result)
        )
        (setq idx (1+ idx))
      )
    )
  )
  (if (> (sslength result) 0) result nil)
)

(defun inhl:index-string (count / idx result)
  (setq idx 0
        result "")
  (while (< idx count)
    (setq result (strcat result (if (= result "") "" " ") (itoa idx)))
    (setq idx (1+ idx))
  )
  result
)

(defun inhl:selected-layout-names (tile-value layout-names / indexes result idx)
  (if (and tile-value (/= tile-value ""))
    (progn
      (setq indexes (read (strcat "(" tile-value ")")))
      (setq result nil)
      (foreach idx indexes
        (if (and (numberp idx) (nth idx layout-names))
          (setq result (cons (nth idx layout-names) result))
        )
      )
      (reverse result)
    )
    layout-names
  )
)

(defun inhl:layout-selection-string (selected-layouts layout-names / idx result name)
  (setq idx 0
        result "")
  (foreach name layout-names
    (if (or (null selected-layouts) (inhl:list-contains-ci name selected-layouts))
      (setq result (strcat result (if (= result "") "" " ") (itoa idx)))
    )
    (setq idx (1+ idx))
  )
  result
)

(defun inhl:entity-handle (ent / dxf)
  (setq dxf (if ent (entget ent) nil))
  (if dxf (cdr (assoc 5 dxf)) nil)
)

(defun inhl:entity-list-contains-p (ent entities / handle found item)
  (setq handle (inhl:entity-handle ent)
        found nil)
  (foreach item entities
    (if (and handle (= handle (inhl:entity-handle item)))
      (setq found t)
    )
  )
  found
)

(defun inhl:ss-from-entities (entities / ss ent)
  (setq ss (ssadd))
  (foreach ent entities
    (if (entget ent)
      (ssadd ent ss)
    )
  )
  (if (> (sslength ss) 0) ss nil)
)

(defun inhl:get-or-create-order-layer ( / acad-obj doc layers layer-obj)
  (setq acad-obj (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad-obj)
        layers (vla-get-layers doc))
  (setq layer-obj (vl-catch-all-apply 'vla-item (list layers "DAC_Plotter_Order")))
  (if (vl-catch-all-error-p layer-obj)
    (setq layer-obj (vla-add layers "DAC_Plotter_Order"))
  )
  (vl-catch-all-apply 'vla-put-color (list layer-obj 2))
  (vl-catch-all-apply 'vla-put-plottable (list layer-obj :vlax-false))
  layer-obj
)

(defun inhl:clear-order-labels ( / ss idx)
  (setq ss (ssget "X" '((8 . "DAC_Plotter_Order"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (entdel (ssname ss idx))
        (setq idx (1+ idx))
      )
    )
  )
)

(defun inhl:frame-info-from-entity (ent / dxf ent-type layout-name pt1 pt2 obj result minpt maxpt p1 p2 w h center)
  (setq dxf (if ent (entget ent) nil))
  (if dxf
    (progn
      (setq ent-type (cdr (assoc 0 dxf))
            layout-name (inhl:entity-layout-name dxf))
      (cond
        ((= ent-type "LINE")
          (setq pt1 (cdr (assoc 10 dxf))
                pt2 (cdr (assoc 11 dxf)))
          (setq p1 (list (min (car pt1) (car pt2)) (min (cadr pt1) (cadr pt2)))
                p2 (list (max (car pt1) (car pt2)) (max (cadr pt1) (cadr pt2))))
        )
        (t
          (setq obj (vlax-ename->vla-object ent))
          (setq result (vl-catch-all-apply 'vla-getboundingbox (list obj 'minpt 'maxpt)))
          (if (not (vl-catch-all-error-p result))
            (progn
              (setq p1 (vlax-safearray->list minpt)
                    p2 (vlax-safearray->list maxpt))
              (setq p1 (list (car p1) (cadr p1))
                    p2 (list (car p2) (cadr p2)))
            )
          )
        )
      )
      (if (and p1 p2)
        (progn
          (setq w (abs (- (car p2) (car p1)))
                h (abs (- (cadr p2) (cadr p1)))
                center (list (+ (car p1) (* 0.5 w)) (+ (cadr p1) (* 0.5 h))))
          (if (and (> w 0.1) (> h 0.1))
            (list ent p1 p2 center w h layout-name)
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun inhl:draw-order-label (frame order / center w h layout-name height)
  (if frame
    (progn
      (inhl:get-or-create-order-layer)
      (setq center (nth 3 frame)
            w (nth 4 frame)
            h (nth 5 frame)
            layout-name (nth 6 frame)
            height (* 0.55 (min w h)))
      (if (< height 1.0) (setq height 1.0))
      (entmake
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          (cons 67 (if (inhl:model-layout-p layout-name) 0 1))
          (cons 410 layout-name)
          '(8 . "DAC_Plotter_Order")
          '(62 . 2)
          '(100 . "AcDbText")
          (cons 10 (list (car center) (cadr center) 0.0))
          (cons 40 height)
          (cons 1 (itoa order))
          '(50 . 0.0)
          '(41 . 0.85)
          '(7 . "Standard")
          '(71 . 0)
          '(72 . 1)
          (cons 11 (list (car center) (cadr center) 0.0))
          '(210 0.0 0.0 1.0)
          '(100 . "AcDbText")
          '(73 . 2)
        )
      )
    )
  )
)

(defun inhl:redraw-manual-order-labels ( / valid ent frame idx)
  (inhl:clear-order-labels)
  (setq valid nil
        idx 1)
  (foreach ent *inhl-manual-drawings*
    (setq frame (inhl:frame-info-from-entity ent))
    (if frame
      (progn
        (setq valid (append valid (list ent)))
        (inhl:draw-order-label frame idx)
        (setq idx (1+ idx))
      )
    )
  )
  (setq *inhl-manual-drawings* valid)
  (length *inhl-manual-drawings*)
)

(defun inhl:select-manual-drawings ( / picked ent dxf ent-type count)
  (setq *inhl-manual-drawings* nil)
  (inhl:clear-order-labels)
  (princ "\n-> Chọn các block khung tên theo đúng thứ tự cần in. Nhấn Enter để kết thúc.")
  (while
    (setq picked
      (entsel
        (strcat
          "\nChọn drawing số "
          (itoa (1+ (length *inhl-manual-drawings*)))
          " <Enter để xong>: "
        )
      )
    )
    (setq ent (car picked)
          dxf (entget ent)
          ent-type (if dxf (cdr (assoc 0 dxf)) nil))
    (cond
      ((null dxf)
        (princ "\n-> Đối tượng không hợp lệ.")
      )
      ((/= ent-type "INSERT")
        (princ "\n-> Chỉ chọn block khung tên (INSERT).")
      )
      ((inhl:entity-list-contains-p ent *inhl-manual-drawings*)
        (princ "\n-> Drawing này đã có trong danh sách.")
      )
      ((null (inhl:frame-info-from-entity ent))
        (princ "\n-> Không đọc được kích thước drawing này.")
      )
      (t
        (setq *inhl-manual-drawings* (append *inhl-manual-drawings* (list ent)))
        (setq count (inhl:redraw-manual-order-labels))
        (princ (strcat "\n-> Đã chọn drawing số " (itoa count) "."))
      )
    )
  )
  (setq count (inhl:redraw-manual-order-labels))
  (princ (strcat "\n-> Tổng số drawing đã chọn: " (itoa count)))
  count
)

(defun inhl:reset-manual-drawings ( / )
  (setq *inhl-manual-drawings* nil)
  (inhl:clear-order-labels)
  (sssetfirst nil nil)
  (redraw)
  (princ "\n-> Đã reset danh sách drawings và xóa STT trên bản vẽ.")
  0
)

(defun inhl:get-plot-state (layout)
  (list
    (vl-catch-all-apply 'vla-get-configname (list layout))
    (vl-catch-all-apply 'vla-get-canonicalmedianame (list layout))
    (vl-catch-all-apply 'vla-get-plotrotation (list layout))
    (vl-catch-all-apply 'vla-get-plottype (list layout))
    (vl-catch-all-apply 'vla-get-usestandardscale (list layout))
    (vl-catch-all-apply 'vla-get-standardscale (list layout))
    (vl-catch-all-apply 'vla-get-centerplot (list layout))
    (vl-catch-all-apply 'vla-get-stylesheet (list layout))
    (vl-catch-all-apply 'vla-get-plotwithplotstyles (list layout))
    (vl-catch-all-apply 'vla-get-plotwithlineweights (list layout))
    (vl-catch-all-apply 'vla-get-scalelineweights (list layout))
  )
)

(defun inhl:restore-plot-state (layout state / value)
  (if (and layout state)
    (progn
      (setq value (nth 0 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-configname (list layout value)))
      (setq value (nth 1 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-canonicalmedianame (list layout value)))
      (setq value (nth 2 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-plotrotation (list layout value)))
      (setq value (nth 3 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-plottype (list layout value)))
      (setq value (nth 4 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-usestandardscale (list layout value)))
      (setq value (nth 5 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-standardscale (list layout value)))
      (setq value (nth 6 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-centerplot (list layout value)))
      (setq value (nth 7 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-stylesheet (list layout value)))
      (setq value (nth 8 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-plotwithplotstyles (list layout value)))
      (setq value (nth 9 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-plotwithlineweights (list layout value)))
      (setq value (nth 10 state))
      (if (not (vl-catch-all-error-p value)) (vl-catch-all-apply 'vla-put-scalelineweights (list layout value)))
    )
  )
)

(defun inhl:setup-window-plot (layout p1 p2 ctb-style)
  (vla-SetWindowToPlot layout (inhl:point2d p1) (inhl:point2d p2))
  (vla-put-PlotType layout 4) ; acWindow
  (vla-put-UseStandardScale layout :vlax-true)
  (vla-put-StandardScale layout 0) ; acScaleToFit
  (vl-catch-all-apply 'vla-put-PlotOrigin (list layout (inhl:point2d '(0.0 0.0))))
  (vla-put-CenterPlot layout :vlax-true)
  (vla-put-PlotWithPlotStyles layout :vlax-true)
  (vl-catch-all-apply 'vla-put-PlotWithLineweights (list layout :vlax-true))
  (vl-catch-all-apply 'vla-put-ScaleLineweights (list layout :vlax-false))
  (if (and ctb-style (/= ctb-style "."))
    (vl-catch-all-apply 'vla-put-StyleSheet (list layout ctb-style))
  )
)

(defun inhl:set-layout-to-plot (plot layout-name / arr result)
  (setq arr (vlax-make-safearray vlax-vbString '(0 . 0)))
  (vlax-safearray-put-element arr 0 layout-name)
  (setq result (vl-catch-all-apply 'vla-SetLayoutsToPlot (list plot (vlax-make-variant arr))))
  (not (vl-catch-all-error-p result))
)

(defun inhl:plot-window-to-device (layout layout-name p1 p2 ctb-style / doc active-layout plot result)
  (if (inhl:activate-layout layout-name)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq active-layout (vla-get-ActiveLayout doc))
      (setq plot (vla-get-Plot doc))
      (if (inhl:set-layout-to-plot plot layout-name)
        (progn
          (inhl:setup-window-plot active-layout p1 p2 ctb-style)
          (setq result (vl-catch-all-apply 'vla-PlotToDevice (list plot)))
          (not (vl-catch-all-error-p result))
        )
        nil
      )
    )
    nil
  )
)

(defun inhl:plot-window-to-file (layout layout-name p1 p2 ctb-style pdf-path media-name plot-rotation / doc active-layout plot result)
  (if (inhl:file-exists-p pdf-path) (vl-file-delete pdf-path))
  (if (inhl:activate-layout layout-name)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq active-layout (vla-get-ActiveLayout doc))
      (setq plot (vla-get-Plot doc))
      (if (inhl:set-layout-to-plot plot layout-name)
        (progn
          (if (and media-name (/= media-name ""))
            (vl-catch-all-apply 'vla-put-canonicalmedianame (list active-layout media-name))
          )
          (if plot-rotation
            (vl-catch-all-apply 'vla-put-plotrotation (list active-layout plot-rotation))
          )
          (inhl:setup-window-plot active-layout p1 p2 ctb-style)
          (setq result (vl-catch-all-apply 'vla-PlotToFile (list plot pdf-path)))
          (and (not (vl-catch-all-error-p result)) (inhl:wait-file pdf-path 20))
        )
        nil
      )
    )
    nil
  )
)

(defun inhl:plot-window-to-file-enjicad (layout layout-name p1 p2 ctb-style pdf-path / doc active-layout plot result folders snapshot generated source-pdf changed extra-pdf count plot-success old-plot-to-file old-full-plot-path)
  (if (inhl:file-exists-p pdf-path) (vl-file-delete pdf-path))
  (setq folders
    (list
      (vl-filename-directory pdf-path)
      (getvar "DWGPREFIX")
      (getenv "TEMP")
    )
  )
  (setq snapshot (inhl:pdf-snapshot folders))
  (if (inhl:activate-layout layout-name)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq active-layout (vla-get-ActiveLayout doc))
      (setq plot (vla-get-Plot doc))
      (if (inhl:set-layout-to-plot plot layout-name)
        (progn
          (inhl:setup-window-plot active-layout p1 p2 ctb-style)
          (setq old-plot-to-file (inhl:get-com-property active-layout 'PlotToFile))
          (setq old-full-plot-path (inhl:get-com-property active-layout 'FullPlotPath))
          (inhl:put-com-property active-layout 'PlotToFile :vlax-true)
          (inhl:put-com-property active-layout 'FullPlotPath pdf-path)
          (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list active-layout))
          (setq result (vl-catch-all-apply 'vla-PlotToDevice (list plot)))
          (if (not (vl-catch-all-error-p old-plot-to-file))
            (inhl:put-com-property active-layout 'PlotToFile old-plot-to-file)
          )
          (if (not (vl-catch-all-error-p old-full-plot-path))
            (inhl:put-com-property active-layout 'FullPlotPath old-full-plot-path)
          )
          (if (vl-catch-all-error-p result)
            nil
            (progn
              (setq count 0)
              (while (and (not (inhl:file-exists-p pdf-path)) (null generated) (< count 40))
                (inhl:delay-ms 250)
                (setq generated (inhl:changed-pdf-after-snapshot folders snapshot))
                (setq count (1+ count))
              )
              (setq source-pdf generated)
              (setq changed (inhl:changed-pdfs-after-snapshot folders snapshot))
              (foreach extra-pdf changed
                (if (and
                      (not (= (strcase extra-pdf) (strcase pdf-path)))
                      (not (inhl:path-in-folder-p extra-pdf (vl-filename-directory pdf-path)))
                    )
                  (setq *inhl-enjicad-extra-pdfs* (inhl:add-unique extra-pdf *inhl-enjicad-extra-pdfs*))
                )
              )
              (setq plot-success
                (cond
                  ((inhl:file-exists-p pdf-path)
                    T)
                  (source-pdf
                    (if (inhl:copy-pdf-to-temp source-pdf pdf-path)
                      (progn
                        (if (not (inhl:path-in-folder-p source-pdf (vl-filename-directory pdf-path)))
                          (setq *inhl-enjicad-extra-pdfs* (inhl:add-unique source-pdf *inhl-enjicad-extra-pdfs*))
                        )
                        T
                      )
                      nil
                    ))
                  (T nil)
                )
              )
              (if plot-success
                (inhl:delete-enjicad-extra-pdfs-now folders snapshot (vl-filename-directory pdf-path))
              )
              plot-success
            )
          )
        )
        nil
      )
    )
    nil
  )
)

(defun inhl:ucs-point2d (pt / result)
  (setq result (trans (list (car pt) (cadr pt) 0.0) 0 1))
  (list (car result) (cadr result) 0.0)
)

(defun inhl:plot-window-to-file-command (layout-name printer-name paper-name orientation p1 p2 ctb-style pdf-path / plot-style plot-style-args result cmd-args)
  (if (inhl:file-exists-p pdf-path) (vl-file-delete pdf-path))
  (if (inhl:activate-layout layout-name)
    (progn
      (setq plot-style (if (and ctb-style (/= ctb-style ".")) ctb-style nil))
      (setq plot-style-args
        (if plot-style
          (list "_Y" plot-style "_Y")
          (list "_N" "_Y")
        )
      )
      (setq cmd-args
        (append
          (list
            "_.-PLOT"
            "_Y"
            layout-name
            printer-name
            paper-name
            "_M"
            (if (= orientation "Landscape") "_L" "_P")
            "_N"
            "_W"
            (inhl:ucs-point2d p1)
            (inhl:ucs-point2d p2)
            "_F"
            "_C"
          )
          plot-style-args
          (list
            "_N"
            "_N"
            "_N"
            pdf-path
            "_N"
            "_Y"
          )
        )
      )
      (setq result (vl-catch-all-apply 'vl-cmdf cmd-args))
      (and (not (vl-catch-all-error-p result)) (inhl:wait-file pdf-path 8))
    )
    nil
  )
)

(defun inhl:sort-frames (lst / avg-h row-tolerance rows current-row sorted-list)
  ;; Tính chiều cao trung bình của các khung bản vẽ để làm khoảng sai số gom hàng
  (setq avg-h (/ (apply '+ (mapcar '(lambda (x) (nth 4 x)) lst)) (float (length lst))))
  (setq row-tolerance (* avg-h 0.5)) ; Sai số cho phép thuộc cùng 1 hàng (50% chiều cao)

  ;; Bước 1: Sắp xếp theo trục Y giảm dần (từ trên xuống dưới)
  (setq lst (vl-sort lst '(lambda (a b) (> (cadr (nth 3 a)) (cadr (nth 3 b))))))

  ;; Bước 2: Gom nhóm các khung bản vẽ vào các hàng (row)
  (setq rows nil
        current-row nil)
  (foreach item lst
    (if (null current-row)
      (setq current-row (list item))
      (if (< (abs (- (cadr (nth 3 item)) (cadr (nth 3 (car current-row))))) row-tolerance)
        (setq current-row (cons item current-row)) ; Cùng hàng
        (progn
          (setq rows (cons current-row rows))
          (setq current-row (list item)) ; Hàng mới
        )
      )
    )
  )
  (if current-row (setq rows (cons current-row rows)))
  (setq rows (reverse rows))

  ;; Bước 3: Sắp xếp từng hàng theo trục X tăng dần (từ trái sang phải) và gộp lại
  (setq sorted-list nil)
  (foreach row rows
    (setq row (vl-sort row '(lambda (a b) (< (car (nth 3 a)) (car (nth 3 b))))))
    (setq sorted-list (append sorted-list row))
  )
  sorted-list
)

(defun inhl:sort-frames-by-layout (lst layout-names / result layout-name frames)
  (setq result nil)
  (foreach layout-name layout-names
    (setq frames (vl-remove-if-not '(lambda (x) (= (nth 6 x) layout-name)) lst))
    (if frames
      (setq result (append result (inhl:sort-frames frames)))
    )
  )
  result
)

;;; ==========================================================================
;;; LỆNH 2: DP - THỰC HIỆN IN HÀNG LOẠT (CÓ DIALOG CHỌN CONFIG)
;;; Tự động nhận diện đường chéo từ layer "DAC_Plotter"
;;; ==========================================================================
(defun c:DP ( / ss old-cmdecho old-filedia old-error old-bgplot old-ctab old-tilemode old-nomutt old-layoutregenctl old-regenmode
                old-plot-config old-plot-media old-plot-rotation old-plot-type
                old-use-standard-scale old-standard-scale old-center-plot
                old-style-sheet old-plot-with-plot-styles old-plot-with-lineweights old-scale-lineweights
                doc layout layout-names layout-name current-layout-name current-layout-idx active-layout-name active-paper active-rotation plot-layout plot-state current-plot-layout current-plot-state
                raw-printer-names all-printer-names printer-names current-printer printer-idx
                global-canonicals global-localizeds ctb-names ctb-display-names
                current-ctb ctb-idx dcl-file f dcl-id result
                layer-names block-names layer-idx block-idx frame-mode selected-layer selected-block selected-layouts
                sel-printer-idx sel-paper-idx sel-ctb-idx sel-layer-idx sel-block-idx
                printer-name plot-printer-name paper-size ctb-style
                sample-ent dxf-sample sample-layer ent-type dxf pt1 pt2
                dwg-path dwg-base idx frame-list frame ent obj minpt maxpt p1 p2
                w h center orientation plot-rotation success-count total-count plot-ok initial-papers paper-idx
                frame-landscape frame-media frame-paper frame-paper-localized frame-paper-landscape combine-pdf temp-dir temp-pdf-files temp-pdf final-pdf merge-ok
                enjicad-extra-folders enjicad-session-snapshot
                saved-settings saved-printer saved-paper saved-ctb canonical-paper)
  
  ;; Thiết lập Bẫy lỗi để khôi phục hệ thống khi người dùng Esc
  (setq old-error *error*)
  (defun *error* (msg)
    (inhl:clear-progress)
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (if old-filedia (setvar "FILEDIA" old-filedia))
    (if old-bgplot (setvar "BACKGROUNDPLOT" old-bgplot))
    (if old-nomutt (setvar "NOMUTT" old-nomutt))
    (if old-layoutregenctl (vl-catch-all-apply 'setvar (list "LAYOUTREGENCTL" old-layoutregenctl)))
    (if old-regenmode (vl-catch-all-apply 'setvar (list "REGENMODE" old-regenmode)))
    (if (and current-plot-layout (not *inhl-fast-plot*))
      (inhl:restore-plot-state current-plot-layout current-plot-state)
    )
    (if old-plot-config (vl-catch-all-apply 'vla-put-configname (list layout old-plot-config)))
    (if old-plot-media (vl-catch-all-apply 'vla-put-canonicalmedianame (list layout old-plot-media)))
    (if old-plot-rotation (vl-catch-all-apply 'vla-put-plotrotation (list layout old-plot-rotation)))
    (if old-plot-type (vl-catch-all-apply 'vla-put-plottype (list layout old-plot-type)))
    (if old-use-standard-scale (vl-catch-all-apply 'vla-put-usestandardscale (list layout old-use-standard-scale)))
    (if old-standard-scale (vl-catch-all-apply 'vla-put-standardscale (list layout old-standard-scale)))
    (if old-center-plot (vl-catch-all-apply 'vla-put-centerplot (list layout old-center-plot)))
    (if old-style-sheet (vl-catch-all-apply 'vla-put-stylesheet (list layout old-style-sheet)))
    (if old-plot-with-plot-styles (vl-catch-all-apply 'vla-put-plotwithplotstyles (list layout old-plot-with-plot-styles)))
    (if old-plot-with-lineweights (vl-catch-all-apply 'vla-put-plotwithlineweights (list layout old-plot-with-lineweights)))
    (if old-scale-lineweights (vl-catch-all-apply 'vla-put-scalelineweights (list layout old-scale-lineweights)))
    (if old-tilemode (vl-catch-all-apply 'setvar (list "TILEMODE" old-tilemode)))
    (if old-ctab (vl-catch-all-apply 'setvar (list "CTAB" old-ctab)))
    (setq *error* old-error)
    (princ (strcat "\n-> Đã dừng lệnh. Lỗi: " msg))
    (princ)
  )

  ;; Lưu trạng thái hệ thống
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-filedia (getvar "FILEDIA"))
  (setq old-bgplot (getvar "BACKGROUNDPLOT"))
  (setq old-ctab (getvar "CTAB"))
  (setq old-tilemode (getvar "TILEMODE"))
  (setq old-nomutt (getvar "NOMUTT"))
  (setq old-layoutregenctl (vl-catch-all-apply 'getvar (list "LAYOUTREGENCTL")))
  (if (vl-catch-all-error-p old-layoutregenctl) (setq old-layoutregenctl nil))
  (setq old-regenmode (vl-catch-all-apply 'getvar (list "REGENMODE")))
  (if (vl-catch-all-error-p old-regenmode) (setq old-regenmode nil))
  (setvar "CMDECHO" 0)
  (setvar "FILEDIA" 0)
  (setvar "BACKGROUNDPLOT" 0) ; Tắt in ngầm để ép CAD in tuần tự, không bị nghẽn file
  (vl-catch-all-apply 'setvar (list "LAYOUTREGENCTL" 2))
  (vl-catch-all-apply 'setvar (list "REGENMODE" 0))

  ;; Khởi tạo ActiveX Layout
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layout (vla-get-activelayout doc))
  (setq layout-names (inhl:get-layout-names doc))
  (setq current-layout-name (if (= old-tilemode 1) "Model" old-ctab))
  (setq current-layout-idx (inhl:position-ci current-layout-name layout-names))
  (if (null current-layout-idx) (setq current-layout-idx 0))
  (setq old-plot-config (vla-get-configname layout))
  (setq old-plot-media (vla-get-canonicalmedianame layout))
  (setq old-plot-rotation (vla-get-plotrotation layout))
  (setq old-plot-type (vla-get-plottype layout))
  (setq old-use-standard-scale (vla-get-usestandardscale layout))
  (setq old-standard-scale (vla-get-standardscale layout))
  (setq old-center-plot (vla-get-centerplot layout))
  (setq old-style-sheet (vla-get-stylesheet layout))
  (setq old-plot-with-plot-styles (vla-get-plotwithplotstyles layout))
  (setq old-plot-with-lineweights (vla-get-plotwithlineweights layout))
  (setq old-scale-lineweights (vla-get-scalelineweights layout))

  ;; Không đọc/lưu file config: mỗi lần chạy lấy cấu hình hiện hành trong bản vẽ.
  (setq saved-settings nil
        saved-printer nil
        saved-paper nil
        saved-ctb nil)

  ;; Chuẩn bị dữ liệu trước khi mở DCL để hộp thoại hiện ra đã đầy đủ danh sách.
  (setq raw-printer-names (vl-remove-if '(lambda (x) (or (= x "None") (= x "<None>") (= x ""))) (inhl:get-plotters layout)))
  (setq all-printer-names raw-printer-names)
  (setq all-printer-names (vl-remove-if 'inhl:hidden-plotter-p all-printer-names))
  (setq printer-names all-printer-names)
  (if (null printer-names)
    (progn
      (alert "Lỗi: Không tìm thấy máy in/plotter trong CAD hiện hành. Hãy kiểm tra cấu hình máy in hoặc cài máy in PDF như PDF24.")
      (exit)
    )
  )

  (setq current-printer
    (cond
      ((member (vla-get-configname layout) printer-names)
        (vla-get-configname layout))
      ((inhl:preferred-pdf-plotter printer-names))
      ((member "PDF24" printer-names)
        "PDF24")
      (t
        (vla-get-configname layout))
    )
  )
  (setq printer-idx (vl-position current-printer printer-names))
  (if (null printer-idx) (setq printer-idx 0))

  (setq initial-papers (inhl:get-papers-cached layout (nth printer-idx printer-names)))
  (setq global-canonicals (if initial-papers (car initial-papers) nil)
        global-localizeds (if initial-papers (cadr initial-papers) nil))
  (if (null global-canonicals)
    (progn
      (alert "Không đọc được danh sách khổ giấy thật của máy in đã chọn.")
      (exit)
    )
  )

  (setq paper-idx (inhl:find-paper-index saved-paper global-canonicals global-localizeds))
  (if (null paper-idx)
    (setq paper-idx (inhl:find-paper-index (vla-get-canonicalmedianame layout) global-canonicals global-localizeds))
  )
  (setq paper-idx (inhl:safe-index paper-idx global-canonicals))
  (setq sel-paper-idx paper-idx)
  (setq saved-paper nil)

  (setq ctb-names (inhl:get-ctbs))
  (setq ctb-display-names (cons "Mặc định (None)" ctb-names))
  (setq current-ctb (if (and saved-ctb (member saved-ctb ctb-names))
                      saved-ctb
                      (vla-get-stylesheet layout)))
  (if (and current-ctb (/= current-ctb "") (/= current-ctb ".") (not (inhl:position-ci current-ctb ctb-names)))
    (progn
      (setq ctb-names (cons current-ctb ctb-names))
      (setq ctb-display-names (cons "Mặc định (None)" ctb-names))
    )
  )
  (setq ctb-idx (vl-position current-ctb ctb-names))
  (if ctb-idx
    (setq ctb-idx (1+ ctb-idx))
    (setq ctb-idx 0)
  )

  ;; Chế độ Theo Layer mặc định dùng layer khung do tool tạo ra.
  (inhl:get-or-create-layer)
  (setq layer-names (inhl:get-layer-names doc))
  (setq layer-idx (inhl:position-ci "DAC_Plotter" layer-names))
  (if (null layer-idx) (setq layer-idx 0))
  (setq block-names (inhl:get-block-names))
  (if (null block-names) (setq block-names (list "<Không có block>")))
  (setq block-idx 0)
  (setq frame-mode "layer")
  (setq selected-layouts layout-names)
  (setq sel-printer-idx printer-idx)
  (setq sel-paper-idx (inhl:safe-index sel-paper-idx global-localizeds))
  (setq sel-ctb-idx ctb-idx)
  (setq sel-layer-idx layer-idx)
  (setq sel-block-idx block-idx)

  (setq result 2)
  (while (= result 2)
  ;; 1. Ghi file DCL tạm thời
  (setq dcl-file (vl-filename-mktemp "plot" nil ".dcl"))
  (setq f (open dcl-file "w"))
  (write-line "dac_plotter_dialog : dialog {" f)
  (write-line "    label = \"DAC Plotter | Batch Plot\";" f)
  (write-line "    : boxed_column {" f)
  (write-line "        label = \"Cấu hình Máy in và Khổ giấy\";" f)
  (write-line "        : row {" f)
  (write-line "            : text { label = \"Máy in (Printer/Plotter)\"; width = 24; fixed_width = true; }" f)
  (write-line "            : popup_list { key = \"printer_list\"; width = 36; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "        : row {" f)
  (write-line "            : text { label = \"Khổ giấy (Paper size)\"; width = 24; fixed_width = true; }" f)
  (write-line "            : popup_list { key = \"paper_list\"; width = 36; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "        : row {" f)
  (write-line "            : text { label = \"Nét in (Plot style)\"; width = 24; fixed_width = true; }" f)
  (write-line "            : popup_list { key = \"ctb_list\"; width = 36; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "    }" f)
  (write-line "    : boxed_column {" f)
  (write-line "        label = \"Chọn khung in\";" f)
  (write-line "        : spacer_0 { }" f)
  (write-line "        : row {" f)
  (write-line "            : radio_button { key = \"mode_layer\"; label = \"Theo Layer\"; width = 24; fixed_width = true; }" f)
  (write-line "            : popup_list { key = \"layer_list\"; width = 36; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "        : row {" f)
  (write-line "            : radio_button { key = \"mode_block\"; label = \"Theo Title Block\"; width = 24; fixed_width = true; }" f)
  (write-line "            : popup_list { key = \"block_list\"; width = 36; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "        : row {" f)
  (write-line "            : radio_button { key = \"mode_manual\"; label = \"Theo Khung tên chọn\"; width = 24; fixed_width = true; }" f)
  (write-line "            : row {" f)
  (write-line "                width = 36;" f)
  (write-line "                fixed_width = true;" f)
  (write-line "                : spacer { width = 1.5; fixed_width = true; }" f)
  (write-line (strcat "                : text { key = \"manual_count\"; label = \"Đã chọn:" (itoa (length *inhl-manual-drawings*)) "\"; width = 9; fixed_width = true; alignment = centered; }") f)
  (write-line "                : button { key = \"select_drawings\"; label = \"Chọn khung tên\"; width = 15; fixed_width = true; height = 0.75; fixed_height = true; }" f)
  (write-line "                : button { key = \"reset_drawings\"; label = \"Reset\"; width = 6; fixed_width = true; height = 0.75; fixed_height = true; }" f)
  (write-line "            }" f)
  (write-line "        }" f)
  (write-line "        : spacer { height = 0.05; fixed_height = true; }" f)
  (write-line "    }" f)
  (write-line "    : boxed_column {" f)
  (write-line "        label = \"Chọn vùng in\";" f)
  (write-line "        : row {" f)
  (write-line "            : list_box { key = \"layout_list\"; width = 61; height = 10; multiple_select = true; }" f)
  (write-line "            : button { key = \"select_all_layouts\"; label = \"All\"; mnemonic = \"A\"; width = 6; fixed_width = true; alignment = top; }" f)
  (write-line "        }" f)
  (write-line "        : spacer { height = 0.02; fixed_height = true; }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "        fixed_width = true;" f)
  (write-line "        alignment = centered;" f)
  (write-line "        : button {" f)
  (write-line "            label = \"Bắt đầu in\";" f)
  (write-line "            key = \"accept\";" f)
  (write-line "            width = 18;" f)
  (write-line "            is_default = true;" f)
  (write-line "        }" f)
  (write-line "        : spacer { width = 2; }" f)
  (write-line "        : button {" f)
  (write-line "            label = \"Hủy bỏ\";" f)
  (write-line "            key = \"cancel\";" f)
  (write-line "            width = 18;" f)
  (write-line "            is_cancel = true;" f)
  (write-line "        }" f)
  (write-line "    }" f)
  (write-line "}" f)
  (close f)

  ;; 2. Nạp và hiển thị DCL
  (setq dcl-id (load_dialog dcl-file))
  (if (cond
        ((null dcl-id) t)
        ((< dcl-id 0) t)
      )
    (progn
      (vl-file-delete dcl-file)
      (princ "\n-> Không thể nạp hộp thoại DCL.")
      (exit)
    )
  )
  (if (not (new_dialog "dac_plotter_dialog" dcl-id))
    (progn
      (unload_dialog dcl-id)
      (vl-file-delete dcl-file)
      (exit)
    )
  )

  ;; Nạp danh sách máy in (lọc bỏ các lựa chọn không hợp lệ như None)
  (start_list "printer_list")
  (mapcar 'add_list printer-names)
  (end_list)

  (set_tile "printer_list" (itoa sel-printer-idx))

  ;; Định nghĩa hàm cập nhật danh sách khổ giấy dựa theo máy in được chọn (cực kỳ an toàn)
  (defun update-papers (p-idx / selected-printer papers canonicals localizeds current-paper paper-idx keep-paper)
    (setq p-idx (inhl:safe-index p-idx printer-names))
    (setq selected-printer (nth p-idx printer-names))
    (setq keep-paper (if global-localizeds (nth (inhl:safe-index sel-paper-idx global-localizeds) global-localizeds) nil))
    (setq papers (inhl:get-papers-cached layout selected-printer))
    (if papers
      (progn
        (setq canonicals (car papers)
              localizeds (cadr papers))
        (setq global-canonicals canonicals
              global-localizeds localizeds)
        (setq paper-idx (inhl:find-paper-index keep-paper canonicals localizeds))
        (if (null paper-idx)
          (progn
            (setq current-paper (vla-get-canonicalmedianame layout))
            (setq paper-idx (inhl:find-paper-index current-paper canonicals localizeds))
          )
        )
        (setq paper-idx (inhl:safe-index paper-idx canonicals))
        (start_list "paper_list")
        (mapcar 'add_list localizeds)
        (end_list)
        (set_tile "paper_list" (itoa paper-idx))
        (setq sel-paper-idx paper-idx)
      )
      (progn
        (setq global-canonicals nil
              global-localizeds nil
              sel-paper-idx 0)
        (start_list "paper_list")
        (end_list)
      )
    )
  )

  (start_list "paper_list")
  (mapcar 'add_list global-localizeds)
  (end_list)
  (set_tile "paper_list" (itoa sel-paper-idx))

  (start_list "ctb_list")
  (mapcar 'add_list ctb-display-names)
  (end_list)
  (set_tile "ctb_list" (itoa sel-ctb-idx))
  (setq sel-printer-idx (inhl:safe-index sel-printer-idx printer-names))
  (setq sel-paper-idx (inhl:safe-index sel-paper-idx global-localizeds))
  (setq sel-ctb-idx (inhl:safe-index sel-ctb-idx ctb-display-names))
  (setq sel-layer-idx (inhl:safe-index sel-layer-idx layer-names))
  (setq sel-block-idx (inhl:safe-index sel-block-idx block-names))

  (start_list "layer_list")
  (mapcar 'add_list layer-names)
  (end_list)
  (set_tile "layer_list" (itoa sel-layer-idx))

  (start_list "block_list")
  (mapcar 'add_list block-names)
  (end_list)
  (set_tile "block_list" (itoa sel-block-idx))

  (start_list "layout_list")
  (mapcar 'add_list layout-names)
  (end_list)
  (set_tile "layout_list" (inhl:layout-selection-string selected-layouts layout-names))
  (mode_tile "layout_list" 2)
  (set_tile "manual_count" (strcat "Đã chọn:" (itoa (length *inhl-manual-drawings*))))

  (cond
    ((= frame-mode "manual")
      (set_tile "mode_layer" "0")
      (set_tile "mode_block" "0")
      (set_tile "mode_manual" "1")
      (mode_tile "layer_list" 1)
      (mode_tile "block_list" 1)
    )
    ((= frame-mode "block")
      (set_tile "mode_layer" "0")
      (set_tile "mode_block" "1")
      (set_tile "mode_manual" "0")
      (mode_tile "layer_list" 1)
      (mode_tile "block_list" 0)
    )
    (t
      (set_tile "mode_layer" "1")
      (set_tile "mode_block" "0")
      (set_tile "mode_manual" "0")
      (mode_tile "layer_list" 0)
      (mode_tile "block_list" 1)
    )
  )

  (defun inhl:capture-dp-dialog-state ( / )
    (setq sel-printer-idx (inhl:safe-index (inhl:get-tile-int "printer_list" sel-printer-idx) printer-names))
    (setq sel-paper-idx (inhl:safe-index (inhl:get-tile-int "paper_list" sel-paper-idx) global-localizeds))
    (setq sel-ctb-idx (inhl:safe-index (inhl:get-tile-int "ctb_list" sel-ctb-idx) ctb-display-names))
    (setq frame-mode
      (cond
        ((= (get_tile "mode_manual") "1") "manual")
        ((= (get_tile "mode_block") "1") "block")
        (t "layer")
      )
    )
    (setq sel-layer-idx (inhl:safe-index (inhl:get-tile-int "layer_list" sel-layer-idx) layer-names))
    (setq sel-block-idx (inhl:safe-index (inhl:get-tile-int "block_list" sel-block-idx) block-names))
    (setq selected-layouts (inhl:selected-layout-names (get_tile "layout_list") layout-names))
  )

  ;; Hành động khi tương tác trên Dialog
  (action_tile "printer_list" "(progn (setq sel-printer-idx (inhl:safe-index (atoi $value) printer-names)) (update-papers sel-printer-idx))")
  (action_tile "mode_layer" "(progn (setq frame-mode \"layer\") (set_tile \"mode_layer\" \"1\") (set_tile \"mode_block\" \"0\") (set_tile \"mode_manual\" \"0\") (mode_tile \"layer_list\" 0) (mode_tile \"block_list\" 1))")
  (action_tile "mode_block" "(progn (setq frame-mode \"block\") (set_tile \"mode_layer\" \"0\") (set_tile \"mode_block\" \"1\") (set_tile \"mode_manual\" \"0\") (mode_tile \"layer_list\" 1) (mode_tile \"block_list\" 0))")
  (action_tile "mode_manual" "(progn (setq frame-mode \"manual\") (set_tile \"mode_layer\" \"0\") (set_tile \"mode_block\" \"0\") (set_tile \"mode_manual\" \"1\") (mode_tile \"layer_list\" 1) (mode_tile \"block_list\" 1))")
  (action_tile "layer_list" "(setq sel-layer-idx (inhl:safe-index (atoi $value) layer-names))")
  (action_tile "block_list" "(setq sel-block-idx (inhl:safe-index (atoi $value) block-names))")
  (action_tile "select_all_layouts" "(progn (set_tile \"layout_list\" (inhl:index-string (length layout-names))) (mode_tile \"layout_list\" 2))")
  (action_tile "select_drawings" "(progn (inhl:capture-dp-dialog-state) (setq frame-mode \"manual\") (done_dialog 2))")
  (action_tile "reset_drawings" "(progn (inhl:reset-manual-drawings) (setq frame-mode \"manual\") (set_tile \"manual_count\" \"Đã chọn:0\"))")
  (action_tile "accept" "
    (inhl:capture-dp-dialog-state)
    (if (null global-canonicals)
      (update-papers sel-printer-idx)
    )
    (if global-canonicals
      (progn
        (setq sel-paper-idx (inhl:safe-index sel-paper-idx global-localizeds))
        (done_dialog 1)
      )
      (alert \"Không đọc được danh sách khổ giấy của máy in đã chọn.\")
    )
  ")
  (action_tile "cancel" "(done_dialog 0)")

  ;; Hiển thị hộp thoại
  (setq result (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file) ; Xóa file DCL tạm
  (cond
    ((= result 2)
      (inhl:select-manual-drawings)
    )
  )
  )

  ;; Nếu bấm Bắt đầu in (result = 1)
  (if (= result 1)
    (progn
      (inhl:show-progress "DAC Plotter: Đang chuẩn bị lệnh in...")
      (setq *inhl-enjicad-extra-pdfs* nil)
      (setq sel-printer-idx (inhl:safe-index sel-printer-idx printer-names))
      (setq sel-paper-idx (inhl:safe-index sel-paper-idx global-localizeds))
      (setq sel-ctb-idx (inhl:safe-index sel-ctb-idx ctb-display-names))
      (setq printer-name (if printer-names (nth sel-printer-idx printer-names) nil))
      (setq paper-size (if global-localizeds (nth sel-paper-idx global-localizeds) nil))
      (if (or (null printer-name) (null paper-size))
        (progn
          (alert "Lỗi: Máy in hoặc Khổ giấy không hợp lệ!")
          (exit)
        )
      )
      (if (= sel-ctb-idx 0)
        (setq ctb-style ".")
        (setq ctb-style (nth (1- sel-ctb-idx) ctb-names))
      )
      
      (setq canonical-paper (nth sel-paper-idx global-canonicals))
      (setq combine-pdf (inhl:pdf-file-plotter-p printer-name))
      (setq plot-printer-name (if combine-pdf (inhl:no-viewer-plotter-name printer-name raw-printer-names) printer-name))
      (if combine-pdf
        (if (/= plot-printer-name printer-name)
          (princ (strcat "\n-> Dùng PC3 không mở PDF lẻ: " plot-printer-name))
          (princ "\n-> Cảnh báo: Chưa tìm thấy PC3 DAC_NoViewer_, PDF lẻ có thể tự mở sau khi in.")
        )
      )
      (setq dwg-path (inhl:folder-with-slash (getvar "DWGPREFIX")))
      (if (or (null dwg-path) (= dwg-path ""))
        (setq dwg-path (inhl:folder-with-slash (getenv "TEMP")))
      )
      (setq dwg-base (vl-filename-base (getvar "DWGNAME")))
      (if (or (null dwg-base) (= dwg-base ""))
        (setq dwg-base "DAC_Plotter")
      )
      (if combine-pdf
        (progn
          (inhl:show-progress "DAC Plotter: Đang chuẩn bị PDF tạm...")
          (if (null (inhl:find-qpdf))
            (progn
              (inhl:clear-progress)
              (alert "Không tìm thấy qpdf.exe để ghép PDF. Hãy cài PDF24 hoặc qpdf.")
              (exit)
            )
          )
          (setq temp-dir (strcat dwg-path "_DAC_Plotter_Temp\\"))
          (setq final-pdf (strcat dwg-path dwg-base "_Combined.pdf"))
          (inhl:delete-temp-folder temp-dir)
          (if (not (inhl:ensure-folder temp-dir))
            (progn
              (inhl:clear-progress)
              (alert "Không tạo được thư mục tạm để ghép PDF.")
              (exit)
            )
          )
          (if (inhl:enjicad-p)
            (progn
              (setq enjicad-extra-folders (list dwg-path (getenv "TEMP")))
              (setq enjicad-session-snapshot (inhl:pdf-snapshot enjicad-extra-folders))
            )
          )
        )
      )
      
      (inhl:show-progress "DAC Plotter: Đang quét khung in...")
      (setq ss nil
            frame-list nil)
      (if (null selected-layouts) (setq selected-layouts layout-names))
      (setq selected-layer (if layer-names (nth sel-layer-idx layer-names) nil))
      (setq selected-block (if block-names (nth sel-block-idx block-names) nil))

      (cond
        ((= frame-mode "manual")
          (inhl:redraw-manual-order-labels)
          (foreach ent *inhl-manual-drawings*
            (setq frame (inhl:frame-info-from-entity ent))
            (if (and frame (inhl:list-contains-ci (nth 6 frame) selected-layouts))
              (setq frame-list (append frame-list (list frame)))
            )
          )
        )
        ((= frame-mode "block")
          (if (and selected-block (/= selected-block "<Không có block>"))
            (setq ss (inhl:ssget-title-blocks selected-block))
          )
        )
        (t
          (if selected-layer
            (setq ss (ssget "X" (list '(0 . "LINE,LWPOLYLINE") (cons 8 selected-layer))))
          )
        )
      )

      (if (or frame-list ss)
        (progn
          (if ss
            (progn
              (setq frame-list nil)
              (setq idx 0)
              
              ;; Duyệt qua các đối tượng để xác định tọa độ khung in
              (while (< idx (sslength ss))
                (setq ent (ssname ss idx))
                (setq frame (inhl:frame-info-from-entity ent))
                
                ;; Nhận diện khung in có kích thước hợp lệ
                (if (and frame (inhl:list-contains-ci (nth 6 frame) selected-layouts))
                  (setq frame-list (cons frame frame-list))
                )
                (setq idx (1+ idx))
              )
            )
          )

          (if frame-list
            (progn
              ;; Drawing thủ công giữ đúng thứ tự click; các mode còn lại sắp xếp tự động.
              (inhl:show-progress "DAC Plotter: Đang sắp xếp danh sách in...")
              (if (/= frame-mode "manual")
                (setq frame-list (inhl:sort-frames-by-layout frame-list layout-names))
              )
              
              (setq success-count 0)
              (setq total-count (length frame-list))
              (setq idx 1)
              (setq active-layout-name nil)
              (setq active-paper nil)
              (setq active-rotation nil)
              (setq temp-pdf-files nil)
              (setvar "NOMUTT" 1)
              
              ;; Tiến hành in từng khung bản vẽ
              (foreach frame frame-list
                (setq p1 (nth 1 frame)
                      p2 (nth 2 frame)
                      w  (nth 4 frame)
                      h  (nth 5 frame)
                      layout-name (nth 6 frame)
                      plot-layout (inhl:get-layout-by-name doc layout-name))
                (inhl:show-progress
                  (strcat
                    "DAC Plotter: Đang in "
                    (itoa idx)
                    "/"
                    (itoa total-count)
                    " - Layout "
                    layout-name
                  )
                )
                
                (setq frame-landscape (> w h))
                (setq frame-media (inhl:paper-for-orientation global-canonicals global-localizeds canonical-paper paper-size frame-landscape))
                (setq frame-paper (car frame-media)
                      frame-paper-localized (cadr frame-media)
                      frame-paper-landscape (inhl:paper-landscape-p frame-paper frame-paper-localized))
                ;; Match frame direction with the actual canonical paper direction.
                (if (= frame-landscape frame-paper-landscape)
                  (setq orientation (if frame-landscape "Landscape" "Portrait")
                        plot-rotation 0) ; ac0degrees
                  (setq orientation (if frame-landscape "Landscape" "Portrait")
                        plot-rotation 1) ; ac90degrees
                )
                
                (if plot-layout
                  (progn
                    ;; Cau hinh layout object truc tiep; SetLayoutsToPlot se chi dinh tab can in.
                    (if (not (= active-layout-name layout-name))
                      (progn
                        (if (and current-plot-layout (not *inhl-fast-plot*))
                          (inhl:restore-plot-state current-plot-layout current-plot-state)
                        )
                        (if (not *inhl-fast-plot*)
                          (setq plot-state (inhl:get-plot-state plot-layout)
                                current-plot-layout plot-layout
                                current-plot-state plot-state)
                          (setq current-plot-layout plot-layout
                                current-plot-state nil)
                        )
                        (vl-catch-all-apply 'vla-put-configname (list plot-layout plot-printer-name))
                        (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list plot-layout))
                        (setq active-layout-name layout-name)
                        (setq active-paper nil)
                        (setq active-rotation nil)
                      )
                    )

                    (if (not (= active-paper frame-paper))
                      (progn
                        (vl-catch-all-apply 'vla-put-canonicalmedianame (list plot-layout frame-paper))
                        (setq active-paper frame-paper)
                        (setq active-rotation nil)
                      )
                    )

                    ;; Chi doi huong giay khi can, tranh refresh layout lap lai qua cham.
                    (if (not (= active-rotation plot-rotation))
                      (progn
                        (vl-catch-all-apply 'vla-put-plotrotation (list plot-layout plot-rotation))
                        (setq active-rotation plot-rotation)
                      )
                    )
                    
                    ;; DWG To PDF/AutoCAD PDF: xuat tung trang tam roi ghep thanh 1 file.
                    ;; EnjiCAD dung workaround rieng vi PlotToFile co the bao fail/offset.
                    ;; PDF24 va may in ao khac: van in thang sang thiet bi.
                    (if combine-pdf
                      (progn
                        (setq temp-pdf (strcat temp-dir dwg-base "_" (inhl:pad-str idx 4) ".pdf"))
                        (setq plot-ok
                          (cond
                            ((inhl:enjicad-p)
                              (inhl:plot-window-to-file-enjicad plot-layout layout-name p1 p2 ctb-style temp-pdf))
                            ((inhl:model-layout-p layout-name)
                              (inhl:plot-window-to-file plot-layout layout-name p1 p2 ctb-style temp-pdf frame-paper plot-rotation))
                            (T
                              (or
                                (inhl:plot-window-to-file-command layout-name plot-printer-name frame-paper-localized orientation p1 p2 ctb-style temp-pdf)
                                (inhl:plot-window-to-file plot-layout layout-name p1 p2 ctb-style temp-pdf frame-paper plot-rotation)
                              )
                            )
                          )
                        )
                        (if plot-ok
                          (progn
                            (setq temp-pdf-files (append temp-pdf-files (list temp-pdf)))
                            (setq success-count (1+ success-count))
                          )
                          nil
                        )
                      )
                      (if (inhl:plot-window-to-device plot-layout layout-name p1 p2 ctb-style)
                        (setq success-count (1+ success-count))
                        nil
                      )
                    )

                    ;; Khoi phuc page setup se thuc hien khi doi layout hoac khi ket thuc lenh.
                  )
                  nil
                )
                (setq idx (1+ idx))
              )

              (if (and current-plot-layout (not *inhl-fast-plot*))
                (progn
                  (inhl:restore-plot-state current-plot-layout current-plot-state)
                  (setq current-plot-layout nil
                        current-plot-state nil)
                )
              )

              (if (and combine-pdf (inhl:enjicad-p))
                (progn
                  (setq temp-pdf-files (inhl:collect-temp-pdfs temp-dir))
                  (setq success-count (length temp-pdf-files))
                  (if enjicad-session-snapshot
                    (progn
                      (inhl:record-enjicad-extra-pdfs
                        (inhl:changed-pdfs-after-snapshot enjicad-extra-folders enjicad-session-snapshot)
                        temp-dir
                      )
                      (if *inhl-enjicad-extra-pdfs*
                        (inhl:delete-files *inhl-enjicad-extra-pdfs*)
                      )
                    )
                  )
                )
              )
               
              (if (= success-count 0)
                (progn
                  (inhl:clear-progress)
                  (alert "Không in được khung nào. Hãy kiểm tra máy in/khổ giấy đã chọn.")
                )
                (if combine-pdf
                  (progn
                    (inhl:show-progress "DAC Plotter: Đang ghép PDF...")
                    (setq merge-ok (inhl:combine-pdfs-qpdf temp-pdf-files final-pdf temp-dir))
                    (if merge-ok
                      (progn
                        (if (and (inhl:enjicad-p) enjicad-session-snapshot)
                          (progn
                            (inhl:record-enjicad-extra-pdfs
                              (inhl:changed-pdfs-after-snapshot enjicad-extra-folders enjicad-session-snapshot)
                              temp-dir
                            )
                            (setq *inhl-enjicad-extra-pdfs* (inhl:remove-path-ci final-pdf *inhl-enjicad-extra-pdfs*))
                          )
                        )
                        (if (not (inhl:cleanup-temp-pdfs temp-pdf-files temp-dir))
                          (princ (strcat "\n-> Cảnh báo: Chưa xóa được thư mục tạm: " temp-dir))
                        )
                        (if *inhl-enjicad-extra-pdfs*
                          (progn
                            (inhl:delete-files *inhl-enjicad-extra-pdfs*)
                            (setq *inhl-enjicad-extra-pdfs* nil)
                          )
                        )
                        (inhl:clear-progress)
                        (inhl:open-file final-pdf)
                        (princ (strcat "\n-> Đã ghép PDF thành công: " final-pdf))
                      )
                      (progn
                        (if (and (inhl:enjicad-p) enjicad-session-snapshot)
                          (progn
                            (inhl:record-enjicad-extra-pdfs
                              (inhl:changed-pdfs-after-snapshot enjicad-extra-folders enjicad-session-snapshot)
                              temp-dir
                            )
                            (if *inhl-enjicad-extra-pdfs*
                              (progn
                                (inhl:delete-files *inhl-enjicad-extra-pdfs*)
                                (setq *inhl-enjicad-extra-pdfs* nil)
                              )
                            )
                          )
                        )
                        (inhl:clear-progress)
                        (alert (strcat "Đã xuất PDF tạm nhưng không ghép được thành 1 file.\nXem lỗi tại:\n" temp-dir "_qpdf_error.txt"))
                      )
                    )
                  )
                )
              )
            )
            (alert "Không có khung bản vẽ nào có kích thước hợp lệ.")
          )
        )
        (alert "Không quét được đối tượng in nào.")
      )
    )
    nil
  )

  ;; Khôi phục trạng thái hệ thống ban đầu
  (inhl:clear-progress)
  (setvar "CMDECHO" old-cmdecho)
  (setvar "FILEDIA" old-filedia)
  (setvar "BACKGROUNDPLOT" old-bgplot)
  (setvar "NOMUTT" old-nomutt)
  (if old-layoutregenctl (vl-catch-all-apply 'setvar (list "LAYOUTREGENCTL" old-layoutregenctl)))
  (if old-regenmode (vl-catch-all-apply 'setvar (list "REGENMODE" old-regenmode)))
  (if old-plot-config (vl-catch-all-apply 'vla-put-configname (list layout old-plot-config)))
  (if old-plot-media (vl-catch-all-apply 'vla-put-canonicalmedianame (list layout old-plot-media)))
  (if old-plot-rotation (vl-catch-all-apply 'vla-put-plotrotation (list layout old-plot-rotation)))
  (if old-plot-type (vl-catch-all-apply 'vla-put-plottype (list layout old-plot-type)))
  (if old-use-standard-scale (vl-catch-all-apply 'vla-put-usestandardscale (list layout old-use-standard-scale)))
  (if old-standard-scale (vl-catch-all-apply 'vla-put-standardscale (list layout old-standard-scale)))
  (if old-center-plot (vl-catch-all-apply 'vla-put-centerplot (list layout old-center-plot)))
  (if old-style-sheet (vl-catch-all-apply 'vla-put-stylesheet (list layout old-style-sheet)))
  (if old-plot-with-plot-styles (vl-catch-all-apply 'vla-put-plotwithplotstyles (list layout old-plot-with-plot-styles)))
  (if old-plot-with-lineweights (vl-catch-all-apply 'vla-put-plotwithlineweights (list layout old-plot-with-lineweights)))
  (if old-scale-lineweights (vl-catch-all-apply 'vla-put-scalelineweights (list layout old-scale-lineweights)))
  (if old-tilemode (vl-catch-all-apply 'setvar (list "TILEMODE" old-tilemode)))
  (if old-ctab (vl-catch-all-apply 'setvar (list "CTAB" old-ctab)))
  (setq *error* old-error)
  (princ)
)

(princ)
