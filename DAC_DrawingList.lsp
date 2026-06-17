;;; ==========================================================================
;;; DAC DRAWING LIST - TRICH XUAT DANH MUC BAN VE TU BLOCK KHUNG TEN
;;; Lenh:
;;;   DAL     - Chon block, tao Table trong CAD va xuat Excel XLSX
;;;   DALSYNC - Cap nhat lai Table/XLSX gan nhat tu block khung ten
;;;   DALIMPORTXLSX  - Doc XLSX va cap nhat nguoc attribute theo HANDLE
;;;   DALIMPORTTABLE - Doc Table gan nhat va cap nhat nguoc attribute theo HANDLE
;;;   DALTAGS - Xem tag/value cua mot block mau
;;; ==========================================================================

(vl-load-com)

(setq *ddl-last-handles* nil)
(setq *ddl-last-table* nil)
(setq *ddl-last-xlsx* nil)
(setq *ddl-last-table-point* nil)
(setq *ddl-selected-headers* nil)
(setq *ddl-table-style* nil)
(setq *ddl-interactive-fields* nil)
(setq *ddl-debug-step* "")
(setq *ddl-config-attrs* nil)
(setq *ddl-config-selected* nil)
(setq *ddl-text-style-names* nil)
(setq *ddl-data-block-name* nil)

(defun ddl:step (text)
  (setq *ddl-debug-step* text)
)

(defun ddl:print-error (msg)
  (princ (strcat "\nDAC Drawing List lỗi tại bước: " *ddl-debug-step*))
  (princ (strcat "\nChi tiết: " msg))
  (princ)
)

(defun ddl:system-headers ()
  '("STT" "LAYOUT" "BLOCK" "HANDLE")
)

(defun ddl:hidden-output-headers ()
  '("LAYOUT" "BLOCK" "HANDLE" "_ATTRS")
)

(defun ddl:editable-headers ( / result header)
  (foreach header *ddl-selected-headers*
    (if (not (ddl:list-contains-ci header (ddl:system-headers)))
      (setq result (append result (list header)))
    )
  )
  result
)

(defun ddl:aliases-for-header (header)
  (cond
    ((= header "SO_BAN_VE") '("SO_BAN_VE" "SOBV" "DRAWING_NO" "DRAWING_NUMBER" "DWG_NO" "SHEET_NO" "NO" "MA_BAN_VE"))
    ((= header "TEN_BAN_VE") '("TEN_BAN_VE" "TENBV" "DRAWING_NAME" "DRAWING_TITLE" "TITLE" "SHEET_TITLE" "TEN_BV"))
    ((= header "REVISION") '("REVISION" "REV" "REV_NO" "REVISION_NO" "LAN_SUA_DOI"))
    ((= header "TY_LE") '("TY_LE" "TYLE" "SCALE" "SCALES"))
    ((= header "NGAY") '("NGAY" "DATE" "ISSUE_DATE" "DRAWING_DATE"))
    ((= header "VE") '("VE" "DRAWN" "DRAWN_BY" "DRAFTER" "DESIGNED"))
    ((= header "KIEM") '("KIEM" "CHECKED" "CHECKED_BY" "CHECK"))
    ((= header "DUYET") '("DUYET" "APPROVED" "APPROVED_BY" "APPROVE"))
    (T (list header))
  )
)

(defun ddl:add-unique-ci (value values / found item val-str)
  (if (and value (not (equal value "")))
    (progn
      (setq val-str (strcase value))
      (foreach item values
        (if (and item (= (strcase item) val-str)) (setq found T))
      )
      (if found values (append values (list value)))
    )
    values
  )
)

(defun ddl:list-contains-ci (value values / found item val-str)
  (if (and value values)
    (progn
      (setq val-str (strcase value))
      (foreach item values
        (if (and item (= val-str (strcase item))) (setq found T))
      )
    )
  )
  found
)

(defun ddl:entity-layout-name (dxf / layout)
  (setq layout (cdr (assoc 410 dxf)))
  (if (and layout (/= layout "")) layout "Model")
)

(defun ddl:block-name (ent / dxf obj name)
  (setq dxf (entget ent))
  (if (and dxf (= (cdr (assoc 0 dxf)) "INSERT"))
    (progn
      (setq name (vl-catch-all-apply
                   '(lambda ()
                      (setq obj (vlax-ename->vla-object ent))
                      (if (vlax-property-available-p obj 'EffectiveName)
                        (vlax-get-property obj 'EffectiveName)
                        (cdr (assoc 2 dxf))
                      ))))
      (if (or (null name) (vl-catch-all-error-p name))
        (cdr (assoc 2 dxf))
        name
      )
    )
    ""
  )
)

(defun ddl:get-attributes (ent / dxf next next-dxf tag value result)
  (ddl:step "get-attributes")
  (setq dxf (entget ent))
  (if (and (= (cdr (assoc 0 dxf)) "INSERT") (= (cdr (assoc 66 dxf)) 1))
    (progn
      (setq next (entnext ent))
      (while (and next (/= (cdr (assoc 0 (setq next-dxf (entget next)))) "SEQEND"))
        (if (= (cdr (assoc 0 next-dxf)) "ATTRIB")
          (progn
            (setq tag (cdr (assoc 2 next-dxf))
                  value (cdr (assoc 1 next-dxf)))
            (setq tag (if tag (strcase tag) ""))
            (setq result (append result (list (cons tag (if value value "")))))
          )
        )
        (setq next (entnext next))
      )
    )
  )
  result
)

(defun ddl:attr-value (attrs aliases / found alias pair)
  (foreach alias aliases
    (if (and (null found) (setq pair (assoc (strcase alias) attrs)))
      (setq found (cdr pair))
    )
  )
  (if found found "")
)

(defun ddl:add-mtext-values-to-row (row values / idx)
  (setq idx 1)
  (foreach value values
    (setq row (append row (list (cons (strcat "MTEXT_" (itoa idx)) value)))
          idx (1+ idx))
  )
  row
)

(defun ddl:row-from-block (ent index mtext-values / dxf attrs row pair layout space pt data-ent)
  (if *ddl-interactive-fields*
    (ddl:row-from-interactive-fields ent index *ddl-interactive-fields*)
    (progn
      (setq dxf (entget ent)
            layout (ddl:entity-layout-name dxf)
            space (cdr (assoc 67 dxf)))
      (if (null space) (setq space 0))
      (if (and *ddl-data-block-name* (/= *ddl-data-block-name* ""))
        (progn
          (setq pt (ddl:insert-point ent)
                data-ent (ddl:find-closest-block pt *ddl-data-block-name* layout space))
          (if data-ent
            (setq attrs (ddl:get-attributes data-ent))
            (setq attrs nil)
          )
        )
        (setq attrs (ddl:get-attributes ent))
      )
      (setq row (list
        (cons "STT" (itoa index))
        (cons "LAYOUT" layout)
        (cons "BLOCK" (ddl:block-name ent))
        (cons "HANDLE" (cdr (assoc 5 dxf)))
        (cons "_ATTRS" attrs)
      ))
      (foreach pair attrs
        (setq row (append row (list pair)))
      )
      (if mtext-values
        (setq row (ddl:add-mtext-values-to-row row mtext-values))
      )
      row
    )
  )
)

(defun ddl:wcs->local (p ins rot sx sy / dx dy cos-rot sin-rot rx ry)
  (setq dx (- (car p) (car ins))
        dy (- (cadr p) (cadr ins))
        cos-rot (cos (- rot))
        sin-rot (sin (- rot))
        rx (+ (* dx cos-rot) (* dy sin-rot))
        ry (- (* dy cos-rot) (* dx sin-rot)))
  (list (/ rx sx) (/ ry sy))
)

(defun ddl:local->wcs (local ins rot sx sy / lx ly cos-rot sin-rot rx ry)
  (setq lx (* (car local) sx)
        ly (* (cadr local) sy)
        cos-rot (cos rot)
        sin-rot (sin rot)
        rx (- (* lx cos-rot) (* ly sin-rot))
        ry (+ (* lx sin-rot) (* ly cos-rot)))
  (list (+ rx (car ins)) (+ ry (cadr ins)) 0.0)
)

(defun ddl:find-text-near (pt layout space tolerance / ss idx ent text-pt dist best-ent min-dist dxf)
  (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 410 layout) (cons 67 space))))
  (if ss
    (progn
      (setq idx 0
            min-dist tolerance
            best-ent nil)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx)
              dxf (entget ent)
              text-pt (cdr (assoc 10 dxf))
              dist (distance (list (car pt) (cadr pt)) (list (car text-pt) (cadr text-pt))))
        (if (< dist min-dist)
          (setq min-dist dist
                best-ent ent)
        )
        (setq idx (1+ idx))
      )
    )
  )
  best-ent
)

(defun ddl:find-closest-block (pt block-name layout space / ss idx ent best-ent min-dist dxf test-pt dist)
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 410 layout) (cons 67 space))))
  (if ss
    (progn
      (setq idx 0
            min-dist 1e20
            best-ent nil)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (if (ddl:same-block-p ent block-name)
          (progn
            (setq dxf (entget ent)
                  test-pt (cdr (assoc 10 dxf))
                  dist (distance (list (car pt) (cadr pt)) (list (car test-pt) (cadr test-pt))))
            (if (< dist min-dist)
              (setq min-dist dist
                    best-ent ent)
            )
          )
        )
        (setq idx (1+ idx))
      )
    )
  )
  best-ent
)

(defun ddl:find-closest-attribute-block (pt layout space / ss idx ent best-ent min-dist dxf test-pt dist attrs)
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 410 layout) (cons 67 space))))
  (if ss
    (progn
      (setq idx 0
            min-dist 1e20
            best-ent nil)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx)
              dxf (entget ent))
        (if (and (= (cdr (assoc 66 dxf)) 1)
                 (setq attrs (ddl:get-attributes ent)))
          (progn
            (setq test-pt (cdr (assoc 10 dxf))
                  dist (distance (list (car pt) (cadr pt)) (list (car test-pt) (cadr test-pt))))
            (if (< dist min-dist)
              (setq min-dist dist
                    best-ent ent)
            )
          )
        )
        (setq idx (1+ idx))
      )
    )
  )
  best-ent
)

(defun ddl:interactive-mtext-fields (ins-ent / ins-dxf ins-pt ins-rot ins-sx ins-sy fields picked-ent opt-vec text-pt prompts keys idx)
  (setq ins-dxf (entget ins-ent)
        ins-pt (cdr (assoc 10 ins-dxf))
        ins-rot (cdr (assoc 50 ins-dxf))
        ins-sx (cdr (assoc 41 ins-dxf))
        ins-sy (cdr (assoc 42 ins-dxf)))
  (if (null ins-rot) (setq ins-rot 0.0))
  (if (null ins-sx) (setq ins-sx 1.0))
  (if (null ins-sy) (setq ins-sy 1.0))
  
  (setq prompts (list
                  "\nChọn Text/MText làm Số hiệu bản vẽ (hoặc Enter/Space để bỏ qua): "
                  "\nChọn Text/MText làm Tên bản vẽ Tiếng Việt (hoặc Enter/Space để bỏ qua): "
                  "\nChọn Text/MText làm Tên bản vẽ Ngôn ngữ thứ 2 (hoặc Enter/Space để bỏ qua): "
                )
        keys (list
               "SỐ HIỆU BẢN VẼ"
               "TÊN BẢN VẼ"
               "TÊN BẢN VẼ (NGÔN NGỮ 2)"
             ))
  
  (princ "\n--- ĐỊNH NGHĨA CÁC CỘT CẦN TRÍCH XUẤT ---")
  (setq idx 0)
  (while (< idx (length prompts))
    (setq picked-ent (car (entsel (nth idx prompts))))
    (if picked-ent
      (if (member (cdr (assoc 0 (entget picked-ent))) '("TEXT" "MTEXT"))
        (progn
          (setq text-pt (cdr (assoc 10 (entget picked-ent)))
                opt-vec (ddl:wcs->local text-pt ins-pt ins-rot ins-sx ins-sy)
                fields (append fields (list (list (nth idx keys) opt-vec))))
          (princ (strcat "\n-> Đã ghi nhận cột: " (nth idx keys)))
        )
        (progn
          (princ "\n-> Đối tượng được chọn không phải là TEXT hoặc MTEXT. Hãy chọn lại.")
          (setq idx (1- idx))
        )
      )
    )
    (setq idx (1+ idx))
  )
  fields
)

(defun ddl:row-from-interactive-fields (ent index fields / dxf ins-pt ins-rot ins-sx ins-sy layout space avg-scale tol row pair col-name local-vec target-pt text-ent text-val)
  (setq dxf (entget ent)
        ins-pt (cdr (assoc 10 dxf))
        ins-rot (cdr (assoc 50 dxf))
        ins-sx (cdr (assoc 41 dxf))
        ins-sy (cdr (assoc 42 dxf))
        layout (cdr (assoc 410 dxf))
        space (cdr (assoc 67 dxf)))
  (if (null ins-rot) (setq ins-rot 0.0))
  (if (null ins-sx) (setq ins-sx 1.0))
  (if (null ins-sy) (setq ins-sy 1.0))
  (if (null space) (setq space 0))
  (setq avg-scale (/ (+ (abs ins-sx) (abs ins-sy)) 2.0)
        tol (* 20.0 avg-scale))
  (setq row (list
    (cons "STT" (itoa index))
    (cons "LAYOUT" layout)
    (cons "BLOCK" (ddl:block-name ent))
    (cons "HANDLE" (cdr (assoc 5 dxf)))
  ))
  (foreach pair fields
    (setq col-name (car pair)
          local-vec (cadr pair)
          target-pt (ddl:local->wcs local-vec ins-pt ins-rot ins-sx ins-sy)
          text-ent (ddl:find-text-near target-pt layout space tol)
          text-val (if text-ent (ddl:text-value text-ent) ""))
    (setq row (append row (list (cons col-name text-val))))
  )
  row
)

(defun ddl:valid-title-block-p (ent / dxf)
  (ddl:step "valid-title-block")
  (if ent
    (progn
      (setq dxf (entget ent))
      (= (cdr (assoc 0 dxf)) "INSERT")
    )
  )
)

(defun ddl:same-block-p (ent block-name / name)
  (setq name (ddl:block-name ent))
  (= (strcase name) (strcase block-name))
)

(defun ddl:insert-point (ent / dxf pt)
  (setq dxf (entget ent)
        pt (cdr (assoc 10 dxf)))
  (if pt pt '(0.0 0.0 0.0))
)

(defun ddl:all-title-blocks-by-name (block-name / ss idx ent result)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (if (ddl:same-block-p ent block-name)
          (setq result (append result (list ent)))
        )
        (setq idx (1+ idx))
      )
    )
  )
  result
)

(defun ddl:get-block-height (ent / height)
  (setq height 0.0)
  (vl-catch-all-apply
    '(lambda ( / obj minpt maxpt p1 p2)
       (setq obj (vlax-ename->vla-object ent))
       (vla-getboundingbox obj 'minpt 'maxpt)
       (if (and minpt maxpt)
         (setq p1 (vlax-safearray->list minpt)
               p2 (vlax-safearray->list maxpt)
               height (abs (- (cadr p2) (cadr p1))))
       )
     )
  )
  height
)

(defun ddl:sort-title-blocks (ents / decorated result item ent pt heights valid-heights avg-h row-tolerance sorted-by-y rows current-row sorted-list row)
  (if (null ents)
    nil
    (progn
      ;; Calculate average height of block references to determine row tolerance
      (setq heights (mapcar 'ddl:get-block-height ents)
            valid-heights (vl-remove 0.0 heights)
            avg-h (if valid-heights
                    (/ (apply '+ valid-heights) (float (length valid-heights)))
                    100.0
                  )
            row-tolerance (max (* avg-h 0.5) 10.0)
            decorated nil)
      
      ;; Decorate: list of (list pt ent)
      (foreach ent ents
        (setq pt (ddl:insert-point ent))
        (if pt
          (setq decorated (append decorated (list (list pt ent))))
        )
      )
      
      ;; Sort by Y descending first (top-to-bottom)
      (setq sorted-by-y
        (vl-sort decorated
          '(lambda (a b) (> (cadr (car a)) (cadr (car b))))
        )
      )
      
      ;; Group into rows based on tolerance
      (setq rows nil
            current-row nil)
      (foreach item sorted-by-y
        (if (null current-row)
          (setq current-row (list item))
          (if (< (abs (- (cadr (car item)) (cadr (car (car current-row))))) row-tolerance)
            (setq current-row (cons item current-row))
            (progn
              (setq rows (cons current-row rows))
              (setq current-row (list item))
            )
          )
        )
      )
      (if current-row (setq rows (cons current-row rows)))
      (setq rows (reverse rows))
      
      ;; For each row, sort left-to-right (X ascending)
      (setq sorted-list nil)
      (foreach row rows
        (setq row
          (vl-sort row
            '(lambda (a b) (< (car (car a)) (car (car b))))
          )
        )
        (setq sorted-list (append sorted-list row))
      )
      
      ;; Extract entities
      (foreach item sorted-list
        (setq result (append result (list (cadr item))))
      )
      result
    )
  )
)

(defun ddl:index-string (count / idx result)
  (setq idx 0)
  (while (< idx count)
    (setq result (if result (strcat result " " (itoa idx)) (itoa idx)))
    (setq idx (1+ idx))
  )
  result
)

(defun ddl:indexes-from-string (value / result)
  (if (and value (/= value ""))
    (setq result (vl-catch-all-apply 'read (list (strcat "(" value ")"))))
  )
  (if (vl-catch-all-error-p result) nil result)
)

(defun ddl:command-indexes (value count / cleaned raw result item)
  (setq cleaned (vl-string-translate ",;" "  " value))
  (if (or (= (strcase cleaned) "A") (= (strcase cleaned) "ALL") (= cleaned ""))
    (ddl:indexes-from-string (ddl:index-string count))
    (progn
      (setq raw (ddl:indexes-from-string cleaned))
      (foreach item raw
        (if (and (numberp item) (>= item 1) (<= item count))
          (setq result (append result (list (1- item))))
        )
      )
      result
    )
  )
)

(defun ddl:field-labels (attrs / labels pair text)
  (foreach pair attrs
    (setq text (cdr pair))
    (if (> (strlen text) 60) (setq text (strcat (substr text 1 57) "...")))
    (setq labels (append labels (list (strcat (car pair) " = " text))))
  )
  labels
)

(defun ddl:nth-safe (idx lst)
  (if (and (numberp idx) (>= idx 0) (< idx (length lst)))
    (nth idx lst)
  )
)

(defun ddl:dialog-refresh-list (key items / item)
  (start_list key)
  (foreach item items (add_list item))
  (end_list)
)

(defun ddl:dialog-available-labels ()
  (ddl:field-labels *ddl-config-attrs*)
)

(defun ddl:dialog-selected-labels ( / labels tag pair)
  (foreach tag *ddl-config-selected*
    (setq pair (assoc tag *ddl-config-attrs*))
    (if pair
      (setq labels (append labels (list (strcat (car pair) " = " (cdr pair)))))
      (setq labels (append labels (list tag)))
    )
  )
  labels
)

(defun ddl:dialog-refresh ()
  (if *ddl-config-attrs*
    (progn
      (ddl:dialog-refresh-list "tag_list" (mapcar 'ddl:dcl-safe (ddl:dialog-available-labels)))
      (ddl:dialog-refresh-list "tag_extract" (mapcar 'ddl:dcl-safe (ddl:dialog-selected-labels)))
    )
  )
)

(defun ddl:dialog-add (value / indexes idx pair tag)
  (setq indexes (ddl:indexes-from-string value))
  (foreach idx indexes
    (setq pair (ddl:nth-safe idx *ddl-config-attrs*)
          tag (if pair (car pair) nil))
    (if (and tag (not (ddl:list-contains-ci tag *ddl-config-selected*)))
      (setq *ddl-config-selected* (append *ddl-config-selected* (list tag)))
    )
  )
  (ddl:dialog-refresh)
)

(defun ddl:remove-nth (idx lst / pos result)
  (setq pos 0)
  (foreach item lst
    (if (/= pos idx)
      (setq result (append result (list item)))
    )
    (setq pos (1+ pos))
  )
  result
)

(defun ddl:remove-keys (lst keys / result item)
  (foreach item lst
    (if (not (ddl:list-contains-ci (car item) keys))
      (setq result (append result (list item)))
    )
  )
  result
)

(defun ddl:text-style-names ( / item names name)
  (setq item (tblnext "STYLE" T))
  (while item
    (setq name (cdr (assoc 2 item)))
    (if (and name (/= name ""))
      (setq names (append names (list name)))
    )
    (setq item (tblnext "STYLE"))
  )
  (if names names (list "Standard"))
)

(defun ddl:list-index-ci (value values / idx result val-str)
  (setq idx 0)
  (if (and value values)
    (progn
      (setq val-str (strcase value))
      (foreach item values
        (if (and item (null result) (= val-str (strcase item)))
          (setq result idx)
        )
        (setq idx (1+ idx))
      )
    )
  )
  (if result result 0)
)

(defun ddl:dialog-remove (value / indexes idx)
  (setq indexes (reverse (ddl:indexes-from-string value)))
  (foreach idx indexes
    (if (and (numberp idx) (>= idx 0) (< idx (length *ddl-config-selected*)))
      (setq *ddl-config-selected* (ddl:remove-nth idx *ddl-config-selected*))
    )
  )
  (ddl:dialog-refresh)
)

(defun ddl:swap-nth (lst idx1 idx2 / pos item result)
  (setq pos 0)
  (foreach item lst
    (cond
      ((= pos idx1) (setq result (append result (list (nth idx2 lst)))))
      ((= pos idx2) (setq result (append result (list (nth idx1 lst)))))
      (T (setq result (append result (list item))))
    )
    (setq pos (1+ pos))
  )
  result
)

(defun ddl:dialog-move (value direction / idx)
  (setq idx (car (ddl:indexes-from-string value)))
  (cond
    ((and (= direction -1) idx (> idx 0))
      (setq *ddl-config-selected* (ddl:swap-nth *ddl-config-selected* idx (1- idx))))
    ((and (= direction 1) idx (< idx (1- (length *ddl-config-selected*))))
      (setq *ddl-config-selected* (ddl:swap-nth *ddl-config-selected* idx (1+ idx))))
  )
  (ddl:dialog-refresh)
)

(defun ddl:dcl-safe (text / out idx ch code)
  (setq out ""
        idx 1)
  (while (<= idx (strlen text))
    (setq ch (substr text idx 1)
          code (ascii ch))
    (cond
      ((= ch "\"") (setq out (strcat out "'")))
      ((= ch "\\") (setq out (strcat out "/")))
      ((and (>= code 32) (<= code 126)) (setq out (strcat out ch)))
      (T (setq out (strcat out "_")))
    )
    (setq idx (1+ idx))
  )
  out
)

(defun ddl:temp-dcl-path ( / dir stamp)
  (setq dir (getvar "TEMPPREFIX"))
  (if (or (null dir) (= dir ""))
    (setq dir (getvar "DWGPREFIX"))
  )
  (if (and dir (/= dir "") (/= (substr dir (strlen dir) 1) "\\"))
    (setq dir (strcat dir "\\"))
  )
  (setq stamp (vl-string-translate "." "_" (rtos (getvar "DATE") 2 8)))
  (strcat dir "DAC_DrawingList_Fields_" stamp ".dcl")
)

(defun ddl:default-style ()
  (list
    (cons "FONT" (getvar "TEXTSTYLE"))
    (cons "TEXT_HEIGHT" "2.5")
    (cons "ROW_HEIGHT" "8.0")
    (cons "COL_WIDTH" "35.0")
    (cons "INCLUDE_MTEXT" "0")
    (cons "SECOND_LANG" "ENG")
    (cons "BLOCK_NAME" "")
    (cons "OUTPUT_MODE" "OK")
  )
)

(defun ddl:safe-string (value fallback)
  (if (and value (not (equal value "")))
    value
    fallback
  )
)

(defun ddl:style-value (key / pair fallback)
  (setq pair (assoc key *ddl-table-style*)
        fallback (cdr (assoc key (ddl:default-style))))
  (ddl:safe-string (if pair (cdr pair) nil) fallback)
)

(defun ddl:get-tile-safe (key fallback / value)
  (setq value (vl-catch-all-apply 'get_tile (list key)))
  (if (vl-catch-all-error-p value)
    fallback
    (ddl:safe-string value fallback)
  )
)

(defun ddl:get-popup-index-safe (key / value)
  (setq value (ddl:get-tile-safe key "0"))
  (if (or (null value) (= value "")) 0 (atoi value))
)

(defun ddl:set-table-style (font text-height row-height col-width include-mtext second-lang)
  (setq *ddl-table-style*
    (list
      (cons "FONT" (ddl:safe-string font (getvar "TEXTSTYLE")))
      (cons "TEXT_HEIGHT" (ddl:safe-string text-height "2.5"))
      (cons "ROW_HEIGHT" (ddl:safe-string row-height "8.0"))
      (cons "COL_WIDTH" (ddl:safe-string col-width "35.0"))
      (cons "INCLUDE_MTEXT" (ddl:safe-string include-mtext "0"))
      (cons "SECOND_LANG" (ddl:safe-string second-lang "ENG"))
      (cons "BLOCK_NAME" (ddl:style-value "BLOCK_NAME"))
      (cons "OUTPUT_MODE" (ddl:style-value "OUTPUT_MODE"))
    )
  )
)

(defun ddl:write-field-dcl (path attrs / stream)
  (ddl:step "config-write-dcl")
  (setq stream (open path "w"))
  (if stream
    (progn
      (write-line "ddl_config : dialog {" stream)
      (write-line "  label = \"DAC | Drawing List Extraction\";" stream)
      (write-line "  : column {" stream)
      ;; Block Data section
      (write-line "    : boxed_row {" stream)
      (write-line "      label = \"Block Data\";" stream)
      (write-line "      : column {" stream)
      (write-line "        : row {" stream)
      (write-line "          : text { label = \"Block Name:\"; width = 12; }" stream)
      (write-line "          : popup_list { key = \"block_name\"; width = 20; }" stream)
      (write-line "          spacer_1;" stream)
      (write-line "          : text { label = \"Source: current drawing\"; width = 25; }" stream)
      (write-line "        }" stream)
      (write-line "        spacer;" stream)
      (write-line "      }" stream)
      (write-line "    }" stream)
      ;; Tag Data section
      (if attrs
        (progn
          (write-line "    : boxed_column {" stream)
          (write-line "      label = \"Tag Data\";" stream)
          (write-line "      : column {" stream)
          (write-line "        : row {" stream)
          ;; Left: Available tags
          (write-line "          : column {" stream)
          (write-line "            : text { label = \"Available Tags\"; }" stream)
          (write-line "            : list_box { key = \"tag_list\"; width = 32; height = 10; multiple_select = true; }" stream)
          (write-line "          }" stream)
          ;; Center: Add/Remove buttons
          (write-line "          : column {" stream)
          (write-line "            alignment = centered;" stream)
          (write-line "            fixed_width = true;" stream)
          (write-line "            spacer;" stream)
          (write-line "            spacer;" stream)
          (write-line "            : button { key = \"add\"; label = \"  >>  \"; width = 10; fixed_width = true; alignment = centered; }" stream)
          (write-line "            spacer_1;" stream)
          (write-line "            : button { key = \"remove\"; label = \"  <<  \"; width = 10; fixed_width = true; alignment = centered; }" stream)
          (write-line "            spacer;" stream)
          (write-line "          }" stream)
          ;; Right: Selected tags
          (write-line "          : column {" stream)
          (write-line "            : text { label = \"Selected Tags\"; }" stream)
          (write-line "            : list_box { key = \"tag_extract\"; width = 32; height = 10; }" stream)
          (write-line "          }" stream)
          (write-line "        }" stream)
          (write-line "        spacer;" stream)
          (write-line "      }" stream)
          (write-line "    }" stream)
        )
      )
      ;; Table Format section
      (write-line "    : boxed_row {" stream)
      (write-line "      label = \"Table Format\";" stream)
      (write-line "      : column {" stream)
      (write-line "        : row {" stream)
      ;; Column 1: Second Language
      (write-line "          : column {" stream)
      (write-line "            : text { label = \"Second language\"; }" stream)
      (write-line "            : popup_list { key = \"second_lang\"; width = 16; fixed_width = true; }" stream)
      (write-line "          }" stream)
      (write-line "          spacer_1;" stream)
      ;; Column 2: Text Style
      (write-line "          : column {" stream)
      (write-line "            : text { label = \"Text style\"; }" stream)
      (write-line "            : popup_list { key = \"font\"; width = 16; fixed_width = true; }" stream)
      (write-line "          }" stream)
      (write-line "          spacer_1;" stream)
      ;; Column 3: Text Height
      (write-line "          : column {" stream)
      (write-line "            : text { label = \"Text height\"; }" stream)
      (write-line "            : edit_box { key = \"text_height\"; edit_width = 16; fixed_width = true; }" stream)
      (write-line "          }" stream)
      (write-line "          spacer_1;" stream)
      ;; Column 4: Options (Toggle)
      (write-line "          : column {" stream)
      (write-line "            : text { label = \"\"; }" stream)
      (write-line "            : toggle { key = \"include_mtext\"; label = \"Insert Link Field/ MText\"; }" stream)
      (write-line "          }" stream)
      (write-line "        }" stream)
      (write-line "        spacer;" stream)
      (write-line "      }" stream)
      (write-line "    }" stream)
      ;; Bottom buttons
      (write-line "    : row {" stream)
      (write-line "      : button { key = \"to_excel\"; label = \"Write Excel\"; width = 14; fixed_width = true; }" stream)
      (write-line "      : button { key = \"to_csv\"; label = \"Write CSV\"; width = 14; fixed_width = true; }" stream)
      (write-line "      : button { key = \"to_table\"; label = \"Create Table\"; width = 14; fixed_width = true; }" stream)
      (write-line "      spacer;" stream)
      (write-line "      : button { key = \"accept\"; label = \"OK\"; is_default = true; width = 14; fixed_width = true; }" stream)
      (write-line "      : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; width = 14; fixed_width = true; }" stream)
      (write-line "    }" stream)
      (write-line "  }" stream)
      (write-line "}" stream)
      (close stream)
      T
    )
  )
)

(defun ddl:save-dialog-tiles ()
  (setq *ddl-temp-font-idx* (get_tile "font")
        *ddl-temp-text-height* (get_tile "text_height")
        *ddl-temp-include-mtext* (get_tile "include_mtext")
        *ddl-temp-second-lang-idx* (get_tile "second_lang"))
)

(defun ddl:configure-extraction (attrs block-name / dcl-file dcl-id dcl-result tags style font text-height row-height col-width include-mtext second-lang output-mode item font-idx lang-idx)
  (ddl:step "config-dialog")
  (setq *ddl-table-style*
    (subst (cons "BLOCK_NAME" (ddl:safe-string block-name ""))
      (assoc "BLOCK_NAME" (ddl:default-style))
      (ddl:default-style)
    )
  )
  (ddl:set-table-style
    (ddl:style-value "FONT")
    (ddl:style-value "TEXT_HEIGHT")
    (ddl:style-value "ROW_HEIGHT")
    (ddl:style-value "COL_WIDTH")
    (ddl:style-value "INCLUDE_MTEXT")
    (ddl:style-value "SECOND_LANG")
  )
  (setq dcl-file (ddl:temp-dcl-path))
  (if (ddl:write-field-dcl dcl-file attrs)
    (progn
      (setq *ddl-config-attrs* attrs
            *ddl-config-selected* nil
            *ddl-text-style-names* (ddl:text-style-names))
      (ddl:step "config-load-dialog")
      (setq dcl-id (load_dialog dcl-file))
      (if (and (> dcl-id 0) (new_dialog "ddl_config" dcl-id))
        (progn
          (ddl:step "config-set-defaults")
          (start_list "block_name")
          (add_list (ddl:style-value "BLOCK_NAME"))
          (end_list)
          (set_tile "block_name" "0")
          (ddl:dialog-refresh)
          (set_tile "include_mtext" (ddl:style-value "INCLUDE_MTEXT"))
          
          (start_list "second_lang")
          (add_list "Tiếng Anh")
          (add_list "Tiếng Trung")
          (add_list "Tiếng Nhật")
          (add_list "Tiếng Hàn")
          (end_list)
          (setq lang-idx (cond
            ((= (ddl:style-value "SECOND_LANG") "ENG") "0")
            ((= (ddl:style-value "SECOND_LANG") "CHI") "1")
            ((= (ddl:style-value "SECOND_LANG") "JPN") "2")
            ((= (ddl:style-value "SECOND_LANG") "KOR") "3")
            (T "0")
          ))
          (set_tile "second_lang" lang-idx)
          
          (start_list "font")
          (foreach item *ddl-text-style-names* (add_list item))
          (end_list)
          (set_tile "font" (itoa (ddl:list-index-ci (ddl:style-value "FONT") *ddl-text-style-names*)))
          (set_tile "text_height" (ddl:style-value "TEXT_HEIGHT"))
          
          ;; Khoi tao bien tam luu gia tri tu dialog
          (setq *ddl-temp-font-idx* nil
                *ddl-temp-text-height* nil
                *ddl-temp-include-mtext* nil
                *ddl-temp-second-lang-idx* nil)
          
          (if attrs
            (progn
              (action_tile "add" "(ddl:dialog-add (get_tile \"tag_list\"))")
              (action_tile "remove" "(ddl:dialog-remove (get_tile \"tag_extract\"))")
            )
          )
          (action_tile "to_excel" "(progn (ddl:save-dialog-tiles) (done_dialog 2))")
          (action_tile "to_csv" "(progn (ddl:save-dialog-tiles) (done_dialog 4))")
          (action_tile "to_table" "(progn (ddl:save-dialog-tiles) (done_dialog 3))")
          (action_tile "accept" "(progn (ddl:save-dialog-tiles) (done_dialog 1))")
          (action_tile "cancel" "(done_dialog 0)")
          
          (ddl:step "config-start-dialog")
          (setq dcl-result (start_dialog))
          (if (member dcl-result '(1 2 3 4))
            (progn
              (ddl:step "config-read-result")
              (setq tags *ddl-config-selected*)
              (setq font-idx (if *ddl-temp-font-idx* (atoi *ddl-temp-font-idx*) 0)
                    font (nth font-idx *ddl-text-style-names*)
                    text-height (ddl:safe-string *ddl-temp-text-height* (ddl:style-value "TEXT_HEIGHT"))
                    row-height (ddl:style-value "ROW_HEIGHT")
                    col-width (ddl:style-value "COL_WIDTH")
                    include-mtext (ddl:safe-string *ddl-temp-include-mtext* "0")
                    second-lang (cond
                      ((= *ddl-temp-second-lang-idx* "0") "ENG")
                      ((= *ddl-temp-second-lang-idx* "1") "CHI")
                      ((= *ddl-temp-second-lang-idx* "2") "JPN")
                      ((= *ddl-temp-second-lang-idx* "3") "KOR")
                      (T "ENG")
                    )
                    output-mode (cond
                      ((= dcl-result 2) "EXCEL")
                      ((= dcl-result 4) "CSV")
                      ((= dcl-result 3) "TABLE")
                      (T "OK")
                    ))
              (ddl:set-table-style font text-height row-height col-width include-mtext second-lang)
              (setq *ddl-table-style*
                (append
                  (ddl:remove-keys *ddl-table-style* '("BLOCK_NAME" "OUTPUT_MODE"))
                  (list (cons "BLOCK_NAME" (ddl:safe-string block-name "")) (cons "OUTPUT_MODE" output-mode))
                )
              )
              (setq style *ddl-table-style*)
            )
          )
        )
      )
      (if (> dcl-id 0) (unload_dialog dcl-id))
      (vl-file-delete dcl-file)
    )
  )
  (if (or tags *ddl-interactive-fields*)
    (list (if tags tags (mapcar 'car *ddl-interactive-fields*)) style)
    nil
  )
)

(defun ddl:text-value (ent / dxf type result pair)
  (setq dxf (entget ent)
        type (cdr (assoc 0 dxf)))
  (cond
    ((= type "TEXT") (cdr (assoc 1 dxf)))
    ((= type "MTEXT")
      (setq result "")
      (foreach pair dxf
        (if (or (= (car pair) 1) (= (car pair) 3))
          (setq result (strcat result (cdr pair)))
        )
      )
      result
    )
    (T "")
  )
)

(defun ddl:pick-mtext-values (prompt / ss idx ent values value)
  (princ prompt)
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx)
              value (ddl:text-value ent)
              values (append values (list (if value value "")))
              idx (1+ idx))
      )
    )
  )
  values
)

(defun ddl:pick-first-title-block ( / picked ent)
  (ddl:step "pick-first-before-entsel")
  (while (and (null ent) (setq picked (entsel "\nChọn block khung tên mẫu: ")))
    (ddl:step "pick-first-after-entsel")
    (if (ddl:valid-title-block-p (car picked))
       (setq ent (car picked))
       (princ "\n-> Hãy chọn block INSERT hoặc XRef khung tên mẫu.")
    )
    (ddl:step "pick-first-before-next-loop")
  )
  ent
)

(defun ddl:pick-title-blocks ( / first block-name attrs config selected ents ent rows idx fields style data-block-ent data-attrs)
  (ddl:step "pick-title-blocks")
  (setq first (ddl:pick-first-title-block))
  (if first
    (progn
      (setq block-name (ddl:block-name first)
            attrs (ddl:get-attributes first))
      
      (if (null attrs)
        ;; Truong hop block khong co attribute (vi du XRef)
        (progn
          (setq data-block-ent (car (entsel "\nChọn block chứa Attribute dữ liệu (hoặc Enter/Space để chọn MText thủ công): ")))
          (if (and data-block-ent (ddl:valid-title-block-p data-block-ent) (setq data-attrs (ddl:get-attributes data-block-ent)))
            (progn
              (setq *ddl-data-block-name* (ddl:block-name data-block-ent))
              (princ (strcat "\n-> Đã nhận diện block dữ liệu: " *ddl-data-block-name*))
              (setq config (ddl:configure-extraction data-attrs block-name)
                    selected (car config)
                    style (cadr config))
              (if selected
                (progn
                  (setq *ddl-interactive-fields* nil)
                  (setq *ddl-selected-headers* (append '("STT" "LAYOUT" "BLOCK") selected '("HANDLE")))
                )
              )
            )
            (progn
              (princ "\n-> Chuyển sang chế độ chọn MText thủ công.")
              (setq *ddl-data-block-name* nil)
              (setq fields (ddl:interactive-mtext-fields first))
            )
          )
        )
        ;; Truong hop block co attribute -> Hien popup luon
        (progn
          (setq *ddl-data-block-name* nil)
          (setq config (ddl:configure-extraction attrs block-name)
                selected (car config)
                style (cadr config))
          (if selected
            (progn
              (setq *ddl-interactive-fields* nil)
              (setq *ddl-selected-headers* (append '("STT" "LAYOUT" "BLOCK") selected '("HANDLE")))
            )
          )
        )
      )
      
      (if fields
        (progn
          (setq *ddl-interactive-fields* fields)
          (setq *ddl-selected-headers* (append '("STT" "LAYOUT" "BLOCK") (mapcar 'car fields) '("HANDLE")))
          ;; Show the table style configuration dialog
          (setq config (ddl:configure-extraction nil block-name)
                style (cadr config))
        )
      )
      
      (if *ddl-selected-headers*
        (progn
          (setq ents (ddl:sort-title-blocks (ddl:all-title-blocks-by-name block-name))
                idx 1)
          (foreach ent ents
            (setq rows (append rows (list (ddl:row-from-block ent idx nil)))
                  idx (1+ idx))
          )
          (princ (strcat "\n-> Đã quét " (itoa (length rows)) " khung tên cùng block: " block-name))
        )
        (princ "\nBạn chưa chọn thông tin nào để trích xuất.")
      )
    )
  )
  rows
)

(defun ddl:collect-attribute-tags (rows / tags row attrs pair)
  (foreach row rows
    (setq attrs (cdr (assoc "_ATTRS" row)))
    (foreach pair attrs
      (if (not (ddl:list-contains-ci (car pair) tags))
        (setq tags (append tags (list (car pair))))
      )
    )
  )
  tags
)

(defun ddl:csv-escape (value / text pos)
  (setq text (if value value ""))
  (while (setq pos (vl-string-search "\"" text))
    (setq text (strcat (substr text 1 pos) "\"\"" (substr text (+ pos 2) (strlen text))))
  )
  (strcat "\"" text "\"")
)

(defun ddl:csv-line (values / line value)
  (foreach value values
    (setq line
      (if line
        (strcat line "," (ddl:csv-escape value))
        (ddl:csv-escape value)
      )
    )
  )
  line
)

(defun ddl:row-value (row header / pair found)
  (setq header (strcase header))
  (foreach pair row
    (if (and (not found) (= (strcase (car pair)) header))
      (setq found (cdr pair))
    )
  )
  (if found found "")
)

(defun ddl:visible-headers-for-rows (rows / result header)
  (foreach header (ddl:headers-for-rows rows)
    (if (not (ddl:list-contains-ci header (ddl:hidden-output-headers)))
      (setq result (append result (list header)))
    )
  )
  result
)

(defun ddl:is-drawing-number-header-p (header / h-upper)
  (setq h-upper (strcase header))
  (and
    (not (vl-string-search "TÊN" h-upper))
    (not (vl-string-search "TEN" h-upper))
    (not (vl-string-search "TITLE" h-upper))
    (not (vl-string-search "NAME" h-upper))
    (or
      (= h-upper "SỐ HIỆU BẢN VẼ")
      (= h-upper "SO HIEU BAN VE")
      (= h-upper "SỐ HIỆU")
      (= h-upper "SO HIEU")
      (= h-upper "SOBV")
      (= h-upper "SH")
      (= h-upper "NO")
      (= h-upper "NO.")
      (vl-string-search "NUMBER" h-upper)
      (vl-string-search "NUM" h-upper)
      (vl-string-search "DWG" h-upper)
      (vl-string-search "DRAWING" h-upper)
      (vl-string-search "SHEET" h-upper)
      (and (vl-string-search "SỐ" h-upper) (vl-string-search "HIỆU" h-upper))
      (and (vl-string-search "SO" h-upper) (vl-string-search "HIEU" h-upper))
      (and (vl-string-search "KÝ" h-upper) (vl-string-search "HIỆU" h-upper))
      (and (vl-string-search "KY" h-upper) (vl-string-search "HIEU" h-upper))
      (wcmatch h-upper "*DRAWING*NO*")
      (wcmatch h-upper "*DWG*NO*")
      (wcmatch h-upper "*SHEET*NO*")
      (wcmatch h-upper "*SO*HIEU*")
      (wcmatch h-upper "*SỐ*HIỆU*")
      (wcmatch h-upper "*SO*BAN*VE*")
      (wcmatch h-upper "*SỐ*BẢN*VẼ*")
      (wcmatch h-upper "*NUMBER*")
      (wcmatch h-upper "*KÝ*HIỆU*")
      (wcmatch h-upper "*KY*HIEU*")
    )
  )
)

(defun ddl:drawing-number-header (headers / result header)
  (foreach header headers
    (if (and (null result) (ddl:is-drawing-number-header-p header))
      (setq result header)
    )
  )
  result
)

(defun ddl:table-title-headers (headers / result number-header header)
  (setq number-header (ddl:drawing-number-header headers))
  (foreach header headers
    (if (and (/= header "STT") (/= header number-header))
      (setq result (append result (list header)))
    )
  )
  result
)

(defun ddl:joined-row-value (row headers / result value)
  (foreach header headers
    (setq value (ddl:row-value row header))
    (if (/= value "")
      (setq result (if result (strcat result "\n" value) value))
    )
  )
  (if result result "")
)

(defun ddl:split-string (str delim / pos result)
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos)))
          str (substr str (+ pos (strlen delim) 1) (strlen str)))
  )
  (if (/= str "")
    (setq result (append result (list str)))
  )
  result
)

(defun ddl:join-strings (lst delim / result first-item item)
  (setq first-item T)
  (foreach item lst
    (if first-item
      (setq result item
            first-item nil)
      (setq result (strcat result delim item))
    )
  )
  (if result result "")
)

(defun ddl:table-records (rows / visible number-header title-headers result row record)
  (setq visible (ddl:visible-headers-for-rows rows)
        number-header (ddl:drawing-number-header visible)
        title-headers (ddl:table-title-headers visible))
  (foreach row rows
    (setq record (list
      (ddl:row-value row "STT")
      (if number-header (ddl:row-value row number-header) "")
    ))
    (if title-headers
      (foreach h title-headers
        (setq record (append record (list (ddl:row-value row h))))
      )
      (setq record (append record (list "")))
    )
    (setq record (append record (list (ddl:row-value row "HANDLE"))))
    (setq result (append result (list record)))
  )
  result
)

(defun ddl:headers-for-rows (rows)
  (if *ddl-selected-headers*
    *ddl-selected-headers*
    (ddl:system-headers)
  )
)

(defun ddl:values-for-row (row headers / result header)
  (foreach header headers
    (setq result (append result (list (ddl:row-value row header))))
  )
  result
)

(defun ddl:rows-from-handles (handles / result ent idx)
  (setq idx 1)
  (foreach handle handles
    (setq ent (handent handle))
    (if ent
      (progn
        (setq result (append result (list (ddl:row-from-block ent idx nil))))
        (setq idx (1+ idx))
      )
    )
  )
  result
)

(defun ddl:write-utf8 (path text / stream result)
  (setq stream (vl-catch-all-apply 'vlax-create-object (list "ADODB.Stream")))
  (if (vl-catch-all-error-p stream)
    nil
    (progn
      (vlax-put-property stream 'Type 2)
      (vlax-put-property stream 'Charset "utf-8")
      (vlax-invoke-method stream 'Open)
      (vlax-invoke-method stream 'WriteText text)
      (setq result (vl-catch-all-apply 'vlax-invoke-method (list stream 'SaveToFile path 2)))
      (vlax-invoke-method stream 'Close)
      (vlax-release-object stream)
      (not (vl-catch-all-error-p result))
    )
  )
)

(defun ddl:export-csv (rows path / headers lines row values text)
  (setq headers (ddl:headers-for-rows rows)
        lines (list (ddl:csv-line headers)))
  (foreach row rows
    (setq values (ddl:values-for-row row headers))
    (setq lines (append lines (list (ddl:csv-line values))))
  )
  ;; Them chi thi sep=, o dong dau tien de Excel nhan dien dung dau phay va ma hoa UTF-8 tieng Viet
  (setq text "sep=,\r\n")
  (foreach line lines
    (setq text (strcat text line "\r\n"))
  )
  (ddl:write-utf8 path text)
)

(defun ddl:set-attr-value (ent aliases value / obj attrs updated tag)
  (setq obj (vlax-ename->vla-object ent)
        attrs (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
  (if (not (vl-catch-all-error-p attrs))
    (foreach att attrs
      (setq tag (vl-catch-all-apply 'vlax-get-property (list att 'TagString)))
      (if (and (not (vl-catch-all-error-p tag))
               (not updated)
               (ddl:list-contains-ci (strcase tag) aliases))
        (progn
          (vl-catch-all-apply 'vlax-put-property (list att 'TextString value))
          (setq updated T)
        )
      )
    )
  )
  updated
)

(defun ddl:importable-header-p (header)
  (and
    header
    (/= header "")
    (not (ddl:list-contains-ci header (ddl:system-headers)))
    (/= (strcase header) "_ATTRS")
  )
)

(defun ddl:apply-record-to-block (record / handle ent header pair aliases count layout space pt data-ent target-ent)
  (setq handle (cdr (assoc "HANDLE" record))
        ent (if handle (handent handle) nil)
        count 0)
  (if ent
    (progn
      (setq target-ent ent)
      ;; If the target entity does not have attributes, find the closest block that does
      (if (null (ddl:get-attributes target-ent))
        (progn
          (setq layout (ddl:entity-layout-name (entget ent))
                space (cdr (assoc 67 (entget ent))))
          (if (null space) (setq space 0))
          (setq pt (ddl:insert-point ent))
          (setq data-ent nil)
          ;; Try to find closest block with *ddl-data-block-name* first
          (if (and *ddl-data-block-name* (/= *ddl-data-block-name* ""))
            (setq data-ent (ddl:find-closest-block pt *ddl-data-block-name* layout space))
          )
          ;; Fallback to closest block with attributes
          (if (null data-ent)
            (setq data-ent (ddl:find-closest-attribute-block pt layout space))
          )
          (if data-ent (setq target-ent data-ent))
        )
      )
      (foreach pair record
        (setq header (car pair))
        (if (ddl:importable-header-p header)
          (progn
            (setq aliases (ddl:aliases-for-header header))
            (if (and aliases (ddl:set-attr-value target-ent aliases (cdr pair)))
              (setq count (1+ count))
            )
          )
        )
      )
      count
    )
    0
  )
)

(defun ddl:records-from-grid (headers data / records row idx record col header value)
  (foreach row data
    (setq idx 0
          record nil)
    (foreach value row
      (setq header (nth idx headers))
      (if header
        (setq record (append record (list (cons header value))))
      )
      (setq idx (1+ idx))
    )
    (if (cdr (assoc "HANDLE" record))
      (setq records (append records (list record)))
    )
  )
  records
)

(defun ddl:import-records (records / total record)
  (setq total 0)
  (foreach record records
    (setq total (+ total (ddl:apply-record-to-block record)))
  )
  total
)

;;; ==========================================================================
;;; CAD TABLE HELPERS
;;; ==========================================================================

(defun ddl:get-header-text (key / lang)
  (setq lang (ddl:style-value "SECOND_LANG"))
  (cond
    ((= lang "ENG")
      (cond
        ((= key "TITLE") "DANH MỤC BẢN VẼ\nINDEX SHEET")
        ((= key "STT") "STT\nNo.")
        ((= key "NUMBER") "SỐ HIỆU BẢN VẼ\nSHEET NUMBER")
        ((= key "NAME") "TÊN BẢN VẼ\nSHEET NAME")
      )
    )
    ((= lang "CHI")
      (cond
        ((= key "TITLE") "DANH MỤC BẢN VẼ\n图纸目录")
        ((= key "STT") "STT\n序号")
        ((= key "NUMBER") "SỐ HIỆU BẢN VẼ\n图号")
        ((= key "NAME") "TÊN BẢN VẼ\n图纸名称")
      )
    )
    ((= lang "JPN")
      (cond
        ((= key "TITLE") "DANH MỤC BẢN VẼ\n図面リスト")
        ((= key "STT") "STT\n番号")
        ((= key "NUMBER") "SỐ HIỆU BẢN VẼ\n図面番号")
        ((= key "NAME") "TÊN BẢN VẼ\n図面名称")
      )
    )
    ((= lang "KOR")
      (cond
        ((= key "TITLE") "DANH MỤC BẢN VẼ\n도면목록")
        ((= key "STT") "STT\n번호")
        ((= key "NUMBER") "SỐ HIỆU BẢN VẼ\n도면번호")
        ((= key "NAME") "TÊN BẢN VẼ\n도면명")
      )
    )
    (T ; Fallback to ENG
      (cond
        ((= key "TITLE") "DANH MỤC BẢN VẼ\nINDEX SHEET")
        ((= key "STT") "STT\nNo.")
        ((= key "NUMBER") "SỐ HIỆU BẢN VẼ\nSHEET NUMBER")
        ((= key "NAME") "TÊN BẢN VẼ\nSHEET NAME")
      )
    )
  )
)

(defun ddl:point3d (pt)
  (vlax-3d-point (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
)

(defun ddl:number-setting (key fallback / value number)
  (setq value (ddl:style-value key)
        number (if value (distof value 2) nil))
  (if (and number (> number 0.0)) number fallback)
)

(defun ddl:active-space (doc)
  (if (= (getvar "TILEMODE") 1)
    (vl-catch-all-apply 'vlax-get-property (list doc 'ModelSpace))
    (vl-catch-all-apply 'vlax-get-property (list doc 'PaperSpace))
  )
)

(defun ddl:table-point (table / raw value)
  (setq raw (vl-catch-all-apply 'vlax-get-property (list table 'InsertionPoint)))
  (if (vl-catch-all-error-p raw)
    *ddl-last-table-point*
    (progn
      (setq value (vl-catch-all-apply 'vlax-variant-value (list raw)))
      (if (vl-catch-all-error-p value) (setq value raw))
      (setq value (vl-catch-all-apply 'vlax-safearray->list (list value)))
      (if (vl-catch-all-error-p value)
        *ddl-last-table-point*
        (list (car value) (cadr value) (caddr value))
      )
    )
  )
)

(defun ddl:apply-table-format (table / font text-height cell-margin)
  (setq font (ddl:style-value "FONT")
        text-height (ddl:number-setting "TEXT_HEIGHT" 2.5)
        cell-margin (* 0.5 text-height))
  (if (and font (/= font ""))
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextStyle 1 font)) ; Title
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextStyle 2 font)) ; Header
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextStyle 4 font)) ; Data
    )
  )
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextHeight 7 text-height))
  ;; Set alignments for Title, Header, and Data
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetAlignment 1 5)) ; Title -> Middle Center (5)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetAlignment 2 4)) ; Header -> Middle Left (4)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetAlignment 4 4)) ; Data -> Middle Left (4)
  ;; Set cell margins globally via direct Table properties
  (vl-catch-all-apply 'vlax-put-property (list table 'HorzCellMargin cell-margin))
  (vl-catch-all-apply 'vlax-put-property (list table 'VertCellMargin (* 0.3 text-height)))
)

(defun ddl:fill-table (table rows / records visible number-header title-headers num-cols text-height scale-factor row-height c r record value tag-list font max-len-0 max-len-1 max-len-c val)
  (setq records (ddl:table-records rows)
        visible (ddl:visible-headers-for-rows rows)
        number-header (ddl:drawing-number-header visible)
        title-headers (ddl:table-title-headers visible)
        num-cols (+ 2 (max (length title-headers) 1))
        text-height (ddl:number-setting "TEXT_HEIGHT" 2.5)
        font (ddl:style-value "FONT"))
  
  (princ (strcat "\n-> Thứ tự cột TÊN BẢN VẼ: " (ddl:join-strings title-headers ", ")))
  (vl-catch-all-apply 'vlax-put-property (list table 'RegenerateTableSuppressed :vlax-true))
  
  ;; 1. Format table styles
  (ddl:apply-table-format table)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextHeight 1 (* 1.5 text-height))) ; Title
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextHeight 2 text-height))        ; Header (sync with Data)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetTextHeight 4 text-height))        ; Data
  
  ;; 2. Set row heights scaled by font height
  (setq row-height (max (ddl:number-setting "ROW_HEIGHT" 8.0) (* 3.2 text-height)))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetRowHeight 0 (* 2.2 row-height))) ; Row 0 (Title)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetRowHeight 1 (* 1.8 row-height))) ; Row 1 (Header)
  (setq r 2)
  (while (< r (+ 2 (length records)))
    (vl-catch-all-apply 'vlax-invoke-method (list table 'SetRowHeight r row-height))
    (setq r (1+ r))
  )
  
  ;; 3. Fill Title (Row 0)
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText 0 0 (ddl:get-header-text "TITLE")))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle 0 0 font))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight 0 0 (* 1.5 text-height)))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment 0 0 5))
  
  ;; 4. Fill Header (Row 1)
  (setq tag-list (append (list (if number-header number-header "")) title-headers))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText 1 0
    (strcat (ddl:get-header-text "STT") "{\\H0.0001;HEADERS:" (ddl:join-strings tag-list ",")
            (if (and *ddl-data-block-name* (/= *ddl-data-block-name* ""))
              (strcat ";DATABLOCK:" *ddl-data-block-name*)
              ""
            )
            "}")))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle 1 0 font))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight 1 0 text-height))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment 1 0 5))
  
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText 1 1 (ddl:get-header-text "NUMBER")))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle 1 1 font))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight 1 1 text-height))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment 1 1 5))
  
  ;; Merge title columns in Header if more than 1 title column
  (if (> num-cols 3)
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list table 'MergeCells 1 1 2 (- num-cols 1)))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText 1 2 (ddl:get-header-text "NAME")))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle 1 2 font))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight 1 2 text-height))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment 1 2 5))
    )
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText 1 2 (ddl:get-header-text "NAME")))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle 1 2 font))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight 1 2 text-height))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment 1 2 5))
    )
  )
  
  ;; 5. Fill Data Rows (Row 2 onwards)
  (setq r 2)
  (foreach record records
    (setq c 0)
    (while (< c num-cols)
      (setq value (nth c record))
      (if (= c 0)
        (setq value (strcat value "{\\H0.0001;HANDLE:" (car (reverse record)) "}"))
      )
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetText r c (if value value "")))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextStyle r c font))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellTextHeight r c text-height))
      (vl-catch-all-apply 'vlax-invoke-method (list table 'SetCellAlignment r c (if (= c 0) 5 4)))
      (setq c (1+ c))
    )
    (setq r (1+ r))
  )
  
  ;; 6. Set column widths based on longest text (auto-fit) - done AFTER filling text
  (setq scale-factor (/ text-height 2.5))
  
  ;; STT Column (Col 0)
  (setq max-len-0 4)
  (foreach record records
    (setq val (nth 0 record))
    (setq max-len-0 (max max-len-0 (if val (strlen val) 0)))
  )
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetColumnWidth 0 (max (* 10.0 scale-factor) (* (+ max-len-0 4) text-height 0.75))))
  
  ;; Sheet No Column (Col 1)
  (setq max-len-1 12)
  (foreach record records
    (setq val (nth 1 record))
    (setq max-len-1 (max max-len-1 (if val (strlen val) 0)))
  )
  (vl-catch-all-apply 'vlax-invoke-method (list table 'SetColumnWidth 1 (max (* 28.0 scale-factor) (* (+ max-len-1 4) text-height 0.75))))
  
  ;; Title Columns (Col 2 to second-to-last)
  (setq c 2)
  (while (< c num-cols)
    (setq max-len-c 15)
    (foreach record records
      (setq val (nth c record))
      (setq max-len-c (max max-len-c (if val (strlen val) 0)))
    )
    (vl-catch-all-apply 'vlax-invoke-method (list table 'SetColumnWidth c (max (* 35.0 scale-factor) (* (+ max-len-c 5) text-height 0.75))))
    (setq c (1+ c))
  )
  
  (vl-catch-all-apply 'vlax-put-property (list table 'RegenerateTableSuppressed :vlax-false))
  (vl-catch-all-apply 'vlax-invoke-method (list table 'Update))
  table
)

(defun ddl:create-table-at (rows pt / doc space table row-height col-width visible title-headers num-cols text-height scale-factor)
  (setq doc (vlax-get-property (vlax-get-acad-object) 'ActiveDocument)
        space (ddl:active-space doc)
        text-height (ddl:number-setting "TEXT_HEIGHT" 2.5)
        scale-factor (/ text-height 2.5)
        row-height (max (ddl:number-setting "ROW_HEIGHT" 8.0) (* 3.2 text-height))
        col-width (* (ddl:number-setting "COL_WIDTH" 35.0) scale-factor)
        visible (ddl:visible-headers-for-rows rows)
        title-headers (ddl:table-title-headers visible)
        num-cols (+ 2 (max (length title-headers) 1))
        table (vl-catch-all-apply
                'vlax-invoke-method
                (list space 'AddTable (ddl:point3d pt) (+ 2 (length rows)) num-cols row-height col-width)))
  (if (vl-catch-all-error-p table)
    nil
    (progn
      (setq *ddl-last-table* table
            *ddl-last-table-point* pt)
      (ddl:fill-table table rows)
    )
  )
)

(defun ddl:export-table (rows / pt)
  (setq pt (getpoint "\nChọn điểm đặt bảng danh mục bản vẽ: "))
  (if pt (ddl:create-table-at rows pt))
)

(defun ddl:refresh-table (rows / pt)
  (setq pt (if *ddl-last-table* (ddl:table-point *ddl-last-table*) nil))
  (if (null pt) (setq pt *ddl-last-table-point*))
  (if *ddl-last-table*
    (vl-catch-all-apply 'vlax-invoke-method (list *ddl-last-table* 'Delete))
  )
  (setq *ddl-last-table* nil)
  (if pt
    (ddl:create-table-at rows pt)
    (ddl:export-table rows)
  )
)

(defun ddl:parse-cell-handle (text / pos res)
  (if (and text (setq pos (vl-string-search "{\\H0.0001;HANDLE:" text)))
    (progn
      (setq res (substr text (+ pos 18)))
      (if (= (substr res (strlen res) 1) "}")
        (setq res (substr res 1 (1- (strlen res))))
      )
      res
    )
    ""
  )
)

(defun ddl:parse-cell-headers (text / pos res semipos header-str block-str bpos)
  (if (and text (setq pos (vl-string-search "{\\H0.0001;HEADERS:" text)))
    (progn
      (setq res (substr text (+ pos 19)))
      (if (= (substr res (strlen res) 1) "}")
        (setq res (substr res 1 (1- (strlen res))))
      )
      (if (setq semipos (vl-string-search ";" res))
        (progn
          (setq header-str (substr res 1 semipos)
                block-str (substr res (+ semipos 2)))
          (if (setq bpos (vl-string-search "DATABLOCK:" block-str))
            (setq *ddl-data-block-name* (substr block-str (+ bpos 11)))
          )
          (ddl:split-string header-str ",")
        )
        (progn
          (setq *ddl-data-block-name* nil)
          (ddl:split-string res ",")
        )
      )
    )
    nil
  )
)

(defun ddl:strip-cell-metadata (text / pos)
  (if (and text (setq pos (vl-string-search "{\\H0.0001;" text)))
    (substr text 1 pos)
    (if text text "")
  )
)

(defun ddl:read-table-records (table / rows cols r c headers data row value stt-text clean-stt parsed-handle tags)
  (setq rows (vl-catch-all-apply 'vlax-get-property (list table 'Rows))
        cols (vl-catch-all-apply 'vlax-get-property (list table 'Columns)))
  (if (or (vl-catch-all-error-p rows) (vl-catch-all-error-p cols))
    nil
    (progn
      ;; Read STT header cell (1, 0) to parse headers metadata
      (setq stt-text (vl-catch-all-apply 'vlax-invoke-method (list table 'GetText 1 0)))
      (if (vl-catch-all-error-p stt-text) (setq stt-text ""))
      (setq tags (ddl:parse-cell-headers stt-text))
      
      (if tags
        (progn
          (setq headers (append (list "STT") tags (list "HANDLE")))
          (setq r 2)
          (while (< r rows)
            (setq row nil)
            ;; Read STT cell (r, 0)
            (setq value (vl-catch-all-apply 'vlax-invoke-method (list table 'GetText r 0)))
            (if (vl-catch-all-error-p value) (setq value ""))
            (setq clean-stt (ddl:strip-cell-metadata value)
                  parsed-handle (ddl:parse-cell-handle value)
                  row (list clean-stt))
            
            ;; Read other visible columns
            (setq c 1)
            (while (< c cols)
              (setq value (vl-catch-all-apply 'vlax-invoke-method (list table 'GetText r c)))
              (setq row (append row (list (if (vl-catch-all-error-p value) "" value)))
                    c (1+ c))
            )
            ;; Append HANDLE at the end
            (setq row (append row (list parsed-handle)))
            (setq data (append data (list row)))
            (setq r (1+ r))
          )
          (ddl:records-from-grid headers data)
        )
        nil
      )
    )
  )
)

;;; ==========================================================================
;;; EXCEL XLSX HELPERS
;;; ==========================================================================

(defun ddl:excel-cleanup (excel book)
  (if book (vl-catch-all-apply 'vlax-invoke-method (list book 'Close :vlax-false)))
  (if excel
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list excel 'Quit))
      (vl-catch-all-apply 'vlax-release-object (list excel))
    )
  )
)

(defun ddl:excel-cell (sheet row col / cells cell)
  (setq cells (vl-catch-all-apply 'vlax-get-property (list sheet 'Cells)))
  (if (not (vl-catch-all-error-p cells))
    (progn
      (setq cell (vl-catch-all-apply 'vlax-get-property (list cells 'Item row col)))
      (if (vl-catch-all-error-p cell)
        (setq cell (vl-catch-all-apply 'vlax-get-property (list cells "Item" row col)))
      )
      (if (vl-catch-all-error-p cell)
        (setq cell (vl-catch-all-apply 'vlax-invoke-method (list cells 'Item row col)))
      )
      (if (vl-catch-all-error-p cell)
        (setq cell (vl-catch-all-apply 'vlax-invoke-method (list cells "Item" row col)))
      )
    )
  )
  cell
)

(defun ddl:excel-cell-value (sheet row col / cells cell value)
  (setq cells (vl-catch-all-apply 'vlax-get-property (list sheet 'Cells)))
  (if (not (vl-catch-all-error-p cells))
    (progn
      (setq cell (vl-catch-all-apply 'vlax-get-property (list cells 'Item row col)))
      (if (vl-catch-all-error-p cell)
        (setq cell (vl-catch-all-apply 'vlax-get-property (list cells "Item" row col)))
      )
      (if (vl-catch-all-error-p cell)
        (setq cell (vl-catch-all-apply 'vlax-invoke-method (list cells 'Item row col)))
      )
      (if (and cell (not (vl-catch-all-error-p cell)))
        (progn
          (setq value (vl-catch-all-apply 'vlax-get-property (list cell 'Value2)))
          (if (vl-catch-all-error-p value)
            (setq value (vl-catch-all-apply 'vlax-get-property (list cell 'Value)))
          )
          (if (vl-catch-all-error-p value) nil value)
        )
      )
    )
  )
)

(defun ddl:excel-put-cell (sheet row col value / cells cell result)
  (setq cells (vl-catch-all-apply 'vlax-get-property (list sheet 'Cells)))
  (if (vl-catch-all-error-p cells)
    (progn
      (princ (strcat "\n-> Lỗi lấy ô Excel (dòng " (itoa row) ", cột " (itoa col) "): " (vl-catch-all-error-message cells)))
      nil
    )
    (progn
      ;; Thử đặt giá trị trực tiếp qua Item của Cells (rất nhanh và tương thích tốt)
      (setq result (vl-catch-all-apply 'vlax-put-property (list cells 'Item row col value)))
      (if (vl-catch-all-error-p result)
        (setq result (vl-catch-all-apply 'vlax-put-property (list cells "Item" row col value)))
      )
      (if (vl-catch-all-error-p result)
        ;; Nếu ghi trực tiếp thất bại, lấy cell object rồi ghi
        (progn
          (setq cell (ddl:excel-cell sheet row col))
          (if (and cell (not (vl-catch-all-error-p cell)))
            (progn
              (setq result (vl-catch-all-apply 'vlax-put-property (list cell 'Value value)))
              (if (vl-catch-all-error-p result)
                (setq result (vl-catch-all-apply 'vlax-put-property (list cell 'Value2 value)))
              )
              (if (vl-catch-all-error-p result)
                (setq result (vl-catch-all-apply 'vlax-put (list cell 'Value value)))
              )
              (if (vl-catch-all-error-p result)
                (setq result (vl-catch-all-apply 'vlax-put (list cell 'Value2 value)))
              )
            )
            (setq result cell)
          )
        )
      )
      (if (vl-catch-all-error-p result)
        (progn
          (princ (strcat "\n-> Lỗi ghi ô Excel (dòng " (itoa row) ", cột " (itoa col) ", giá trị \"" (vl-princ-to-string value) "\"): " (vl-catch-all-error-message result)))
          nil
        )
        T
      )
    )
  )
)

(defun ddl:get-subtitle-text ()
  (cond
    ((= (ddl:style-value "SECOND_LANG") "ENG") "INDEX SHEET")
    ((= (ddl:style-value "SECOND_LANG") "CHI") "图纸目录")
    ((= (ddl:style-value "SECOND_LANG") "JPN") "図面リスト")
    ((= (ddl:style-value "SECOND_LANG") "KOR") "도면목록")
    (T "INDEX SHEET")
  )
)

(defun ddl:get-stt-translation ()
  (cond
    ((= (ddl:style-value "SECOND_LANG") "ENG") "NO.")
    ((= (ddl:style-value "SECOND_LANG") "CHI") "序号")
    ((= (ddl:style-value "SECOND_LANG") "JPN") "番号")
    ((= (ddl:style-value "SECOND_LANG") "KOR") "번호")
    (T "NO.")
  )
)

(defun ddl:get-number-translation ()
  (cond
    ((= (ddl:style-value "SECOND_LANG") "ENG") "SHEET NUMBER")
    ((= (ddl:style-value "SECOND_LANG") "CHI") "图号")
    ((= (ddl:style-value "SECOND_LANG") "JPN") "図面番号")
    ((= (ddl:style-value "SECOND_LANG") "KOR") "도면번호")
    (T "SHEET NUMBER")
  )
)

(defun ddl:get-name-translation ()
  (cond
    ((= (ddl:style-value "SECOND_LANG") "ENG") "SHEET NAME")
    ((= (ddl:style-value "SECOND_LANG") "CHI") "图纸名称")
    ((= (ddl:style-value "SECOND_LANG") "JPN") "図面名称")
    ((= (ddl:style-value "SECOND_LANG") "KOR") "도면명")
    (T "SHEET NAME")
  )
)

(defun ddl:excel-set-second-line-red (sheet row col first-line second-line / cell chars font start len text)
  (setq text (strcat first-line "\n" second-line))
  (ddl:excel-put-cell sheet row col text)
  (setq cell (ddl:excel-cell sheet row col))
  (if (and cell (not (vl-catch-all-error-p cell)))
    (progn
      (setq start (+ (strlen first-line) 2) ; 1-indexed, plus 1 for '\n'
            len (strlen second-line))
      (setq chars (vl-catch-all-apply 'vlax-get-property (list cell 'Characters start len)))
      (if (and chars (not (vl-catch-all-error-p chars)))
        (progn
          (setq font (vl-catch-all-apply 'vlax-get-property (list chars 'Font)))
          (if (and font (not (vl-catch-all-error-p font)))
            (vl-catch-all-apply 'vlax-put-property (list font 'Color 255)) ; RGB Red (255)
          )
        )
      )
    )
  )
)

(defun ddl:excel-header-to-key (header / h-upper)
  (setq h-upper (strcase header))
  (cond
    ((wcmatch h-upper "*HANDLE*") "HANDLE")
    ((wcmatch h-upper "*STT*") "STT")
    ((or (wcmatch h-upper "*SỐ*HIỆU*") (wcmatch h-upper "*SO*HIEU*") (wcmatch h-upper "*SHEET*NUMBER*") (wcmatch h-upper "*DRAWING*NUMBER*") (wcmatch h-upper "*NUMBER*"))
      "SỐ HIỆU BẢN VẼ")
    ((wcmatch h-upper "*2ND*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*ENG*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*CHI*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*JPN*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*KOR*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*图纸名称*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*図面名称*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((wcmatch h-upper "*도면명*") "TÊN BẢN VẼ (NGÔN NGỮ 2)")
    ((or (wcmatch h-upper "*TÊN*BẢN*VẼ*") (wcmatch h-upper "*TEN*BAN*VE*") (wcmatch h-upper "*SHEET*NAME*") (wcmatch h-upper "*DRAWING*NAME*"))
      "TÊN BẢN VẼ")
    (T header)
  )
)

(defun ddl:excel-style-range (sheet start-row start-col end-row end-col / cell1 cell2 range)
  (setq cell1 (ddl:excel-cell sheet start-row start-col)
        cell2 (ddl:excel-cell sheet end-row end-col))
  (if (and cell1 cell2 (not (vl-catch-all-error-p cell1)) (not (vl-catch-all-error-p cell2)))
    (progn
      (setq range (vl-catch-all-apply 'vlax-get-property (list sheet 'Range cell1 cell2)))
      (if (and range (not (vl-catch-all-error-p range)))
        range
      )
    )
  )
)

(defun ddl:export-xlsx (rows path / excel books book sheets sheet visible-headers headers r c row header values value columns result native-path total-cols range borders font interior rows-coll cols-coll row-item col-item ext fmt-code title-text total-rows cell1 cell2 title2-idx cell entcol inside-horiz h-upper has-lang2)
  (setq native-path (vl-string-translate "/" "\\" path))
  (setq excel (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))
  (if (vl-catch-all-error-p excel)
    (progn
      (alert (strcat "Lỗi: Không khởi tạo được Excel. Máy của bạn có thể chưa cài đặt Microsoft Excel hoặc cấu hình COM bị lỗi.\n\nChi tiết: " (vl-catch-all-error-message excel)))
      nil
    )
    (progn
      (vl-catch-all-apply 'vlax-put-property (list excel 'DisplayAlerts :vlax-false))
      (setq books (vl-catch-all-apply 'vlax-get-property (list excel 'Workbooks)))
      (if (vl-catch-all-error-p books)
        (progn
          (alert (strcat "Lỗi khi truy cập danh sách Workbooks: " (vl-catch-all-error-message books)))
          (ddl:excel-cleanup excel nil)
          nil
        )
        (progn
          (setq book (vl-catch-all-apply 'vlax-invoke-method (list books 'Add)))
          (if (vl-catch-all-error-p book)
            (progn
              (alert (strcat "Lỗi khi tạo Workbook mới: " (vl-catch-all-error-message book)))
              (ddl:excel-cleanup excel nil)
              nil
            )
            (progn
              (setq sheets (vl-catch-all-apply 'vlax-get-property (list book 'Worksheets))
                    sheet (if (vl-catch-all-error-p sheets) sheets (vl-catch-all-apply 'vlax-get-property (list sheets 'Item 1))))
              (if (vl-catch-all-error-p sheet)
                (progn
                  (alert (strcat "Lỗi khi truy cập Worksheet đầu tiên: " (vl-catch-all-error-message sheet)))
                  (ddl:excel-cleanup excel book)
                  nil
                )
                (progn
                  (princ "\n-> Đang khởi tạo dữ liệu và ghi vào Excel...")
                  
                  ;; Columns setup:
                  ;; Col 1: HANDLE (Hidden)
                  ;; Col 2+: visible-headers
                  (setq visible-headers (ddl:visible-headers-for-rows rows))
                  
                  ;; Force headers order to always be: HANDLE, STT, SỐ HIỆU BẢN VẼ, TÊN BẢN VẼ, TÊN BẢN VẼ (NGÔN NGỮ 2)
                  (setq headers (list "HANDLE" "STT" "SỐ HIỆU BẢN VẼ" "TÊN BẢN VẼ"))
                  (setq has-lang2 nil)
                  (foreach h visible-headers
                    (if (= (strcase h) "TÊN BẢN VẼ (NGÔN NGỮ 2)")
                      (setq has-lang2 T)
                    )
                  )
                  (if has-lang2
                    (setq headers (append headers (list "TÊN BẢN VẼ (NGÔN NGỮ 2)")))
                  )
                  ;; Append other custom/extra headers that are not part of the standard set
                  (foreach h visible-headers
                    (setq h-upper (strcase h))
                    (if (not (member h-upper '("STT" "SỐ HIỆU BẢN VẼ" "TÊN BẢN VẼ" "TÊN BẢN VẼ (NGÔN NGỮ 2)")))
                      (setq headers (append headers (list h)))
                    )
                  )
                  (setq total-cols (length headers))
                  
                  ;; 1. Write Title (Row 1)
                  (ddl:excel-put-cell sheet 1 2 "DANH MỤC BẢN VẼ") ; Start at Col 2 (STT)
                  
                  ;; Merge Title across Col 2 to Col total-cols
                  (setq range (ddl:excel-style-range sheet 1 2 1 total-cols))
                  (if range
                    (progn
                      (vl-catch-all-apply 'vlax-invoke-method (list range 'Merge))
                      ;; Style Title
                      (setq font (vl-catch-all-apply 'vlax-get-property (list range 'Font)))
                      (if (and font (not (vl-catch-all-error-p font)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list font 'Name "Arial"))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Size 16))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Bold :vlax-true))
                        )
                      )
                      (vl-catch-all-apply 'vlax-put-property (list range 'HorizontalAlignment -4108))
                      (vl-catch-all-apply 'vlax-put-property (list range 'VerticalAlignment -4108))
                    )
                  )
                  
                  ;; 2. Write Subtitle (Row 2)
                  (ddl:excel-put-cell sheet 2 2 (ddl:get-subtitle-text))
                  
                  ;; Merge Subtitle across Col 2 to Col total-cols
                  (setq range (ddl:excel-style-range sheet 2 2 2 total-cols))
                  (if range
                    (progn
                      (vl-catch-all-apply 'vlax-invoke-method (list range 'Merge))
                      ;; Style Subtitle
                      (setq font (vl-catch-all-apply 'vlax-get-property (list range 'Font)))
                      (if (and font (not (vl-catch-all-error-p font)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list font 'Name "Arial"))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Size 12))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Bold :vlax-true))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Color 255)) ; RGB Red (255)
                        )
                      )
                      (vl-catch-all-apply 'vlax-put-property (list range 'HorizontalAlignment -4108))
                      (vl-catch-all-apply 'vlax-put-property (list range 'VerticalAlignment -4108))
                    )
                  )
                  
                  ;; Set Row 1 and Row 2 Heights
                  (setq rows-coll (vl-catch-all-apply 'vlax-get-property (list sheet 'Rows)))
                  (if (and rows-coll (not (vl-catch-all-error-p rows-coll)))
                    (progn
                      (setq row-item (vl-catch-all-apply 'vlax-get-property (list rows-coll 'Item 1)))
                      (if (and row-item (not (vl-catch-all-error-p row-item)))
                        (vl-catch-all-apply 'vlax-put-property (list row-item 'RowHeight 30))
                      )
                      (setq row-item (vl-catch-all-apply 'vlax-get-property (list rows-coll 'Item 2)))
                      (if (and row-item (not (vl-catch-all-error-p row-item)))
                        (vl-catch-all-apply 'vlax-put-property (list row-item 'RowHeight 20))
                      )
                    )
                  )
                  
                  ;; 3. Style Headers Row 3 (Col 2 to total-cols) FIRST
                  ;; This is done first so character coloring is not overwritten by range font sizing.
                  (setq range (ddl:excel-style-range sheet 3 2 3 total-cols))
                  (if range
                    (progn
                      (setq font (vl-catch-all-apply 'vlax-get-property (list range 'Font)))
                      (if (and font (not (vl-catch-all-error-p font)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list font 'Name "Arial"))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Size 11))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Bold :vlax-true))
                        )
                      )
                      (setq interior (vl-catch-all-apply 'vlax-get-property (list range 'Interior)))
                      (if (and interior (not (vl-catch-all-error-p interior)))
                        (vl-catch-all-apply 'vlax-put-property (list interior 'ColorIndex 15)) ; Light Gray
                      )
                      (vl-catch-all-apply 'vlax-put-property (list range 'HorizontalAlignment -4108))
                      (vl-catch-all-apply 'vlax-put-property (list range 'VerticalAlignment -4108))
                    )
                  )
                  
                  ;; Set Row 3 Height to 35
                  (if (and rows-coll (not (vl-catch-all-error-p rows-coll)))
                    (progn
                      (setq row-item (vl-catch-all-apply 'vlax-get-property (list rows-coll 'Item 3)))
                      (if (and row-item (not (vl-catch-all-error-p row-item)))
                        (vl-catch-all-apply 'vlax-put-property (list row-item 'RowHeight 35))
                      )
                    )
                  )
                  
                  ;; Write Row 3 cells
                  (ddl:excel-put-cell sheet 3 1 "HANDLE")
                  
                  ;; Column 2: STT
                  (ddl:excel-set-second-line-red sheet 3 2 "STT" (ddl:get-stt-translation))
                  
                  ;; Column 3: NUMBER
                  (ddl:excel-set-second-line-red sheet 3 3 "SỐ HIỆU BẢN VẼ" (ddl:get-number-translation))
                  
                  ;; Column 4: TÊN BẢN VẼ
                  (ddl:excel-set-second-line-red sheet 3 4 "TÊN BẢN VẼ" (ddl:get-name-translation))
                  
                  ;; Write other header cells
                  (setq c 5)
                  (while (<= c total-cols)
                    (setq header (nth (1- c) headers))
                    (if (not (member header '("HANDLE" "STT" "SỐ HIỆU BẢN VẼ" "TÊN BẢN VẼ" "TÊN BẢN VẼ (NGÔN NGỮ 2)")))
                      (ddl:excel-put-cell sheet 3 c header)
                      (ddl:excel-put-cell sheet 3 c "")
                    )
                    (setq c (1+ c))
                  )
                  
                  ;; Merge Title headers if Title 2 is present
                  (if (and (member "TÊN BẢN VẼ (NGÔN NGỮ 2)" headers)
                           (setq title2-idx (1+ (vl-position "TÊN BẢN VẼ (NGÔN NGỮ 2)" headers))))
                    (progn
                      (setq range (ddl:excel-style-range sheet 3 4 3 title2-idx))
                      (if range
                        (vl-catch-all-apply 'vlax-invoke-method (list range 'Merge))
                      )
                    )
                  )
                  
                  ;; 4. Write Data (Row 4+)
                  (setq r 4)
                  (foreach row rows
                    (setq values (ddl:values-for-row row headers)
                          c 1)
                    (foreach value values
                      (ddl:excel-put-cell sheet r c value)
                      (setq c (1+ c))
                    )
                    
                    ;; Set Row height to 20
                    (if (and rows-coll (not (vl-catch-all-error-p rows-coll)))
                      (progn
                        (setq row-item (vl-catch-all-apply 'vlax-get-property (list rows-coll 'Item r)))
                        (if (and row-item (not (vl-catch-all-error-p row-item)))
                          (vl-catch-all-apply 'vlax-put-property (list row-item 'RowHeight 20))
                        )
                      )
                    )
                    (setq r (1+ r))
                  )
                  
                  (setq total-rows (1- r))
                  (princ (strcat "\n-> Đã ghi xong " (itoa (length rows)) " dòng dữ liệu."))
                  
                  ;; 5. Style Data rows
                  ;; STT (Col 2, Row 4 to total-rows)
                  (setq range (ddl:excel-style-range sheet 4 2 total-rows 2))
                  (if range
                    (progn
                      (setq font (vl-catch-all-apply 'vlax-get-property (list range 'Font)))
                      (if (and font (not (vl-catch-all-error-p font)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list font 'Name "Arial"))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Size 11))
                        )
                      )
                      (vl-catch-all-apply 'vlax-put-property (list range 'HorizontalAlignment -4108))
                      (vl-catch-all-apply 'vlax-put-property (list range 'VerticalAlignment -4108))
                    )
                  )
                  ;; Others (Col 3 to total-cols, Row 4 to total-rows)
                  (setq range (ddl:excel-style-range sheet 4 3 total-rows total-cols))
                  (if range
                    (progn
                      (setq font (vl-catch-all-apply 'vlax-get-property (list range 'Font)))
                      (if (and font (not (vl-catch-all-error-p font)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list font 'Name "Arial"))
                          (vl-catch-all-apply 'vlax-put-property (list font 'Size 11))
                        )
                      )
                      (vl-catch-all-apply 'vlax-put-property (list range 'HorizontalAlignment -4131)) ; Left
                      (vl-catch-all-apply 'vlax-put-property (list range 'VerticalAlignment -4108))
                    )
                  )
                  
                  ;; 6a. Outer borders for Title & Subtitle range (Row 1 to Row 2, Col 2 to total-cols)
                  ;; with inside horizontal border set to xlNone to remove dividing line.
                  (setq range (ddl:excel-style-range sheet 1 2 2 total-cols))
                  (if range
                    (progn
                      (setq borders (vl-catch-all-apply 'vlax-get-property (list range 'Borders)))
                      (if (and borders (not (vl-catch-all-error-p borders)))
                        (progn
                          ;; Set all borders to xlContinuous / xlThin
                          (vl-catch-all-apply 'vlax-put-property (list borders 'LineStyle 1))
                          (vl-catch-all-apply 'vlax-put-property (list borders 'Weight 2))
                          ;; Explicitly remove inside horizontal borders (between Row 1 and Row 2)
                          (setq inside-horiz (vl-catch-all-apply 'vlax-get-property (list borders 'Item 12))) ; xlInsideHorizontal = 12
                          (if (and inside-horiz (not (vl-catch-all-error-p inside-horiz)))
                            (vl-catch-all-apply 'vlax-put-property (list inside-horiz 'LineStyle -4142)) ; xlNone = -4142
                          )
                        )
                      )
                    )
                  )
                  
                  ;; 6b. Borders for Headers and Data (Row 3 to total-rows, Col 2 to total-cols)
                  (setq range (ddl:excel-style-range sheet 3 2 total-rows total-cols))
                  (if range
                    (progn
                      (setq borders (vl-catch-all-apply 'vlax-get-property (list range 'Borders)))
                      (if (and borders (not (vl-catch-all-error-p borders)))
                        (progn
                          (vl-catch-all-apply 'vlax-put-property (list borders 'LineStyle 1)) ; xlContinuous = 1
                          (vl-catch-all-apply 'vlax-put-property (list borders 'Weight 2))     ; xlThin = 2
                        )
                      )
                    )
                  )
                  
                  ;; 7. AutoFit Column Widths (Col 2 onwards, using Row 4+ data cells to avoid merged headers/titles)
                  (setq cell1 (ddl:excel-cell sheet 4 2)
                        cell2 (ddl:excel-cell sheet total-rows total-cols))
                  (if (and cell1 cell2 (not (vl-catch-all-error-p cell1)) (not (vl-catch-all-error-p cell2)))
                    (progn
                      (setq range (vl-catch-all-apply 'vlax-get-property (list sheet 'Range cell1 cell2)))
                      (if (and range (not (vl-catch-all-error-p range)))
                        (progn
                          (setq columns (vl-catch-all-apply 'vlax-get-property (list range 'Columns)))
                          (if (and columns (not (vl-catch-all-error-p columns)))
                            (vl-catch-all-apply 'vlax-invoke-method (list columns 'AutoFit))
                          )
                        )
                      )
                    )
                  )
                  
                  ;; 8. Hide Column 1 (HANDLE) completely
                  (setq cols-coll (vl-catch-all-apply 'vlax-get-property (list sheet 'Columns)))
                  (if (and cols-coll (not (vl-catch-all-error-p cols-coll)))
                    (progn
                      ;; Try getting column via "A"
                      (setq col-item (vl-catch-all-apply 'vlax-get-property (list cols-coll 'Item "A")))
                      ;; Fallback to 1
                      (if (or (null col-item) (vl-catch-all-error-p col-item))
                        (setq col-item (vl-catch-all-apply 'vlax-get-property (list cols-coll 'Item 1)))
                      )
                      (if (and col-item (not (vl-catch-all-error-p col-item)))
                        (progn
                          ;; Set ColumnWidth to 0.0 to hide it visually
                          (vl-catch-all-apply 'vlax-put-property (list col-item 'ColumnWidth 0.0))
                          ;; Set Hidden to True
                          (vl-catch-all-apply 'vlax-put-property (list col-item 'Hidden :vlax-true))
                        )
                      )
                    )
                  )
                  
                  ;; 9. Save and cleanup
                  (setq ext (strcase (vl-filename-extension native-path))
                        fmt-code (if (= ext ".XLS") 56 51))
                  (princ (strcat "\n-> Đã hoàn thành định dạng Excel. Đang lưu file với định dạng " ext "..."))
                  (setq result (vl-catch-all-apply 'vlax-invoke (list book 'SaveAs native-path fmt-code)))
                  (if (vl-catch-all-error-p result)
                    (setq result (vl-catch-all-apply 'vlax-invoke-method (list book 'SaveAs native-path fmt-code)))
                  )
                  (if (vl-catch-all-error-p result)
                    (setq result (vl-catch-all-apply 'vlax-invoke-method (list book 'SaveAs native-path)))
                  )
                  (if (vl-catch-all-error-p result)
                    (alert (strcat "Lỗi khi lưu file Excel:\nĐường dẫn: " native-path "\n\nChi tiết lỗi: " (vl-catch-all-error-message result)))
                  )
                  (ddl:excel-cleanup excel book)
                  (not (vl-catch-all-error-p result))
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun ddl:read-xlsx-records (path / excel books book sheets sheet row col headers data values value done native-path h-str key last-key empty-count)
  (setq native-path (vl-string-translate "/" "\\" path))
  (setq excel (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))
  (if (vl-catch-all-error-p excel)
    nil
    (progn
      (setq books (vl-catch-all-apply 'vlax-get-property (list excel 'Workbooks)))
      (if (vl-catch-all-error-p books)
        (progn (ddl:excel-cleanup excel nil) nil)
        (progn
          (setq book (vl-catch-all-apply 'vlax-invoke-method (list books 'Open native-path)))
          (if (vl-catch-all-error-p book)
            (progn (ddl:excel-cleanup excel nil) nil)
            (progn
              (setq sheets (vl-catch-all-apply 'vlax-get-property (list book 'Worksheets))
                    sheet (if (vl-catch-all-error-p sheets) sheets (vl-catch-all-apply 'vlax-get-property (list sheets 'Item 1))))
              ;; Read headers from Row 3 (Row 1 is Title, Row 2 is Subtitle, Row 3 is Header row)
              (setq col 1 last-key "" empty-count 0)
              (while (and (not (vl-catch-all-error-p sheet)) (< col 100) (< empty-count 3))
                (setq value (ddl:excel-cell-value sheet 3 col))
                (if (or (null value) (= (vl-princ-to-string value) ""))
                  (progn
                    (if (or (= last-key "TÊN BẢN VẼ") (= last-key "TEN_BAN_VE"))
                      (setq key "TÊN BẢN VẼ (NGÔN NGỮ 2)")
                      (setq key "")
                    )
                    (setq empty-count (1+ empty-count))
                  )
                  (progn
                    (setq key (ddl:excel-header-to-key (vl-princ-to-string value))
                          empty-count 0)
                  )
                )
                (setq headers (append headers (list key))
                      last-key key
                      col (1+ col))
              )
              ;; Read data starting from Row 4 (Row 1 Title, Row 2 Subtitle, Row 3 Header)
              (setq row 4)
              (while (and (< row 5000) (not done))
                (setq col 1 values nil)
                (while (<= col (length headers))
                  (setq value (ddl:excel-cell-value sheet row col))
                  (setq values (append values (list (if value (vl-princ-to-string value) "")))
                        col (1+ col))
                )
                (if (= (apply 'strcat values) "")
                  (setq done T)
                  (setq data (append data (list values))
                        row (1+ row))
                )
              )
              (ddl:excel-cleanup excel book)
              (ddl:records-from-grid headers data)
            )
          )
        )
      )
    )
  )
)

(defun c:DALIMPORTXLSX ( / path records total)
  (setq path (getfiled "Chọn file Excel danh mục bản vẽ" (if *ddl-last-xlsx* *ddl-last-xlsx* (getvar "DWGPREFIX")) "xlsx" 0))
  (if path
    (progn
      (setq records (ddl:read-xlsx-records path))
      (if records
        (progn
          (setq total (ddl:import-records records))
          (princ (strcat "\n-> Đã cập nhật " (itoa total) " attribute từ Excel."))
        )
        (alert "Không đọc được XLSX. Hãy kiểm tra Excel, file có đang mở hoặc thiếu cột HANDLE không.")
      )
    )
  )
  (princ)
)

(defun c:DALTAGS ( / picked ent attrs pair)
  (setq picked (entsel "\nChọn block khung tên mẫu để xem tag: "))
  (if picked
    (progn
      (setq ent (car picked))
      (if (= (cdr (assoc 0 (entget ent))) "INSERT")
        (progn
          (setq attrs (ddl:get-attributes ent))
          (if attrs
            (progn
              (princ "\n--- Attribute tags ---")
              (foreach pair attrs
                (princ (strcat "\n" (car pair) " = " (cdr pair)))
              )
            )
            (princ "\nBlock này không có attribute.")
          )
        )
        (princ "\nĐối tượng được chọn không phải block INSERT.")
      )
    )
  )
  (princ)
)

(defun c:DAL ( / *error* rows default-path xlsx-path table-ok xlsx-ok output-mode)
  (defun *error* (msg) (ddl:print-error msg))
  (ddl:step "command-DAL-start")
  (setq rows (ddl:pick-title-blocks))
  (if rows
    (progn
      (setq output-mode (ddl:style-value "OUTPUT_MODE"))
      (setq *ddl-last-handles* (mapcar '(lambda (row) (cdr (assoc "HANDLE" row))) rows))
      (if (and (/= output-mode "EXCEL") (/= output-mode "CSV"))
        (setq table-ok (ddl:export-table rows))
      )
      (if (or (= output-mode "EXCEL") (= output-mode "CSV") (= output-mode "OK"))
        (progn
          (setq default-path (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME"))
                                     (if (= output-mode "CSV") "_DrawingList.csv" "_DrawingList.xlsx")))
          (setq xlsx-path (getfiled "Lưu danh mục bản vẽ" default-path (if (= output-mode "CSV") "csv" "xlsx;xls") 1))
          (if xlsx-path
            (progn
              (if (= (strcase (vl-filename-extension xlsx-path)) ".CSV")
                (setq xlsx-ok (ddl:export-csv rows xlsx-path))
                (setq xlsx-ok (ddl:export-xlsx rows xlsx-path))
              )
              (if xlsx-ok (setq *ddl-last-xlsx* xlsx-path))
            )
          )
        )
      )
      (cond
        ((and (= output-mode "TABLE") table-ok) (princ "\n-> Đã tạo CAD Table."))
        ((and (= output-mode "EXCEL") xlsx-ok) (princ "\n-> Đã xuất Excel XLSX."))
        ((and (= output-mode "CSV") xlsx-ok) (princ "\n-> Đã xuất danh mục ra file CSV."))
        ((and table-ok xlsx-ok)
          (if (= (strcase (vl-filename-extension *ddl-last-xlsx*)) ".CSV")
            (princ "\n-> Đã tạo CAD Table và xuất file CSV.")
            (princ "\n-> Đã tạo CAD Table và xuất Excel XLSX.")
          )
        )
        (table-ok (princ "\n-> Đã tạo CAD Table. File Excel/CSV chưa xuất hoặc bị lỗi."))
        (xlsx-ok
          (if (= (strcase (vl-filename-extension *ddl-last-xlsx*)) ".CSV")
            (princ "\n-> Đã xuất file CSV. CAD Table chưa tạo hoặc bị lỗi.")
            (princ "\n-> Đã xuất Excel XLSX. CAD Table chưa tạo hoặc bị lỗi.")
          )
        )
        (T (alert "Không tạo được Table hoặc Excel/CSV. Hãy kiểm tra cài đặt."))
      )
    )
    (princ "\nKhông có block khung tên nào được chọn.")
  )
  (princ)
)

(defun c:DALSYNC ( / rows)
  (if *ddl-last-handles*
    (progn
      (setq rows (ddl:rows-from-handles *ddl-last-handles*))
      (if rows
        (progn
          (if *ddl-last-table*
            (ddl:refresh-table rows)
          )
          (if *ddl-last-xlsx*
            (progn
              (if (= (strcase (vl-filename-extension *ddl-last-xlsx*)) ".CSV")
                (ddl:export-csv rows *ddl-last-xlsx*)
                (ddl:export-xlsx rows *ddl-last-xlsx*)
              )
            )
          )
          (princ "\n-> Đã đồng bộ lại Table/XLSX/CSV từ block khung tên.")
        )
        (alert "Không tìm thấy lại các block đã lưu handle.")
      )
    )
    (alert "Chưa có danh sách block gần nhất. Hãy chạy DAL trước.")
  )
  (princ)
)

(defun c:DALIMPORTTABLE ( / picked obj records total obj-name)
  (setq picked (entsel "\nChọn CAD Table danh mục để nhập ngược về khung tên: "))
  (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked))
            obj-name (vl-catch-all-apply 'vlax-get-property (list obj 'ObjectName)))
      (if (and (not (vl-catch-all-error-p obj-name)) (= obj-name "AcDbTable"))
        (progn
          (setq records (ddl:read-table-records obj))
          (setq total (ddl:import-records records))
          (princ (strcat "\n-> Đã cập nhật " (itoa total) " attribute từ CAD Table."))
        )
        (alert "Đối tượng được chọn không phải CAD Table.")
      )
    )
  )
  (princ)
)

(princ "\nDAC Drawing List loaded. Lệnh: DAL, DALSYNC, DALIMPORTTABLE, DALIMPORTXLSX, DALTAGS.")
(princ)
