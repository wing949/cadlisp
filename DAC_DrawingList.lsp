;;; ==========================================================================
;;; DAC DRAWING LIST - TRICH XUAT DANH MUC BAN VE TU BLOCK KHUNG TEN
;;; Lenh:
;;;   DDL     - Chon tung block khung ten va xuat CSV
;;;   DDLALL  - Chon block, tao Table trong CAD va xuat Excel XLSX
;;;   DDLSYNC - Cap nhat lai Table/XLSX gan nhat tu block khung ten
;;;   DDLIMPORTXLSX  - Doc XLSX va cap nhat nguoc attribute theo HANDLE
;;;   DDLIMPORTTABLE - Doc Table gan nhat va cap nhat nguoc attribute theo HANDLE
;;;   DDLTAGS - Xem tag/value cua mot block mau
;;; ==========================================================================

(vl-load-com)

(setq *ddl-last-handles* nil)
(setq *ddl-last-table* nil)
(setq *ddl-last-xlsx* nil)
(setq *ddl-last-table-point* nil)
(setq *ddl-selected-headers* nil)

(defun ddl:system-headers ()
  '("STT" "LAYOUT" "BLOCK" "HANDLE")
)

(defun ddl:editable-headers ()
  (if *ddl-selected-headers*
    (vl-remove-if
      '(lambda (header) (ddl:list-contains-ci header (ddl:system-headers)))
      *ddl-selected-headers*
    )
    nil
  )
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

(defun ddl:add-unique-ci (value values / found item)
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

(defun ddl:list-contains-ci (value values / found item)
  (foreach item values
    (if (= (strcase value) (strcase item)) (setq found T))
  )
  found
)

(defun ddl:entity-layout-name (dxf / layout)
  (setq layout (cdr (assoc 410 dxf)))
  (if (and layout (/= layout "")) layout "Model")
)

(defun ddl:block-name (ent / dxf obj result)
  (setq dxf (entget ent)
        result (cdr (assoc 2 dxf)))
  (if (and (fboundp 'vlax-ename->vla-object) ent)
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq result (vl-catch-all-apply 'vlax-get-property (list obj 'EffectiveName)))
          (if (vl-catch-all-error-p result)
            (setq result (cdr (assoc 2 dxf)))
          )
        )
      )
    )
  )
  (if result result "")
)

(defun ddl:get-attributes (ent / dxf next next-dxf tag value result)
  (setq dxf (entget ent))
  (if (and (= (cdr (assoc 0 dxf)) "INSERT") (= (cdr (assoc 66 dxf)) 1))
    (progn
      (setq next (entnext ent))
      (while (and next (/= (cdr (assoc 0 (setq next-dxf (entget next)))) "SEQEND"))
        (if (= (cdr (assoc 0 next-dxf)) "ATTRIB")
          (progn
            (setq tag (strcase (cdr (assoc 2 next-dxf)))
                  value (cdr (assoc 1 next-dxf)))
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

(defun ddl:row-from-block (ent index / dxf attrs row pair)
  (setq dxf (entget ent)
        attrs (ddl:get-attributes ent))
  (setq row (list
    (cons "STT" (itoa index))
    (cons "LAYOUT" (ddl:entity-layout-name dxf))
    (cons "BLOCK" (ddl:block-name ent))
    (cons "HANDLE" (cdr (assoc 5 dxf)))
    (cons "_ATTRS" attrs)
  ))
  (foreach pair attrs
    (setq row (append row (list pair)))
  )
  row
)

(defun ddl:valid-title-block-p (ent / dxf)
  (if ent
    (progn
      (setq dxf (entget ent))
      (and (= (cdr (assoc 0 dxf)) "INSERT") (ddl:get-attributes ent))
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

(defun ddl:field-labels (attrs / labels pair text)
  (foreach pair attrs
    (setq text (cdr pair))
    (if (> (strlen text) 60) (setq text (strcat (substr text 1 57) "...")))
    (setq labels (append labels (list (strcat (car pair) " = " text))))
  )
  labels
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

(defun ddl:write-field-dcl (path / text)
  (setq text
    (strcat
      "ddl_fields : dialog {\r\n"
      "  label = \"Chọn thông tin trích xuất\";\r\n"
      "  : column {\r\n"
      "    : text { label = \"Chọn các attribute sẽ đưa vào danh mục bản vẽ:\"; }\r\n"
      "    : list_box { key = \"fields\"; width = 70; height = 16; multiple_select = true; }\r\n"
      "    : row {\r\n"
      "      : button { key = \"all\"; label = \"Chọn tất cả\"; }\r\n"
      "      : button { key = \"none\"; label = \"Bỏ chọn\"; }\r\n"
      "      spacer;\r\n"
      "      ok_cancel;\r\n"
      "    }\r\n"
      "  }\r\n"
      "}\r\n"
    )
  )
  (ddl:write-utf8 path text)
)

(defun ddl:choose-fields (attrs / dcl-file dcl-id labels value all-value picked tags idx result)
  (setq labels (ddl:field-labels attrs)
        dcl-file (ddl:temp-dcl-path))
  (if (and labels (ddl:write-field-dcl dcl-file))
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (and (> dcl-id 0) (new_dialog "ddl_fields" dcl-id))
        (progn
          (start_list "fields")
          (foreach label labels (add_list label))
          (end_list)
          (setq all-value (ddl:index-string (length labels))
                value all-value)
          (set_tile "fields" value)
          (action_tile "all" "(setq value all-value)(set_tile \"fields\" value)")
          (action_tile "none" "(setq value \"\")(set_tile \"fields\" \"\")")
          (action_tile "fields" "(setq value $value)")
          (if (= (start_dialog) 1)
            (progn
              (setq picked (ddl:indexes-from-string value))
              (foreach idx picked
                (setq tags (append tags (list (car (nth idx attrs)))))
              )
              (setq result tags)
            )
          )
        )
      )
      (if (> dcl-id 0) (unload_dialog dcl-id))
      (vl-file-delete dcl-file)
    )
  )
  result
)

(defun ddl:pick-first-title-block ( / picked ent)
  (while (and (null ent) (setq picked (entsel "\nChọn block khung tên mẫu: ")))
    (if (ddl:valid-title-block-p (car picked))
      (setq ent (car picked))
      (princ "\n-> Hãy chọn block INSERT có attribute.")
    )
  )
  ent
)

(defun ddl:pick-title-blocks ( / first attrs selected picked ent dxf type rows idx)
  (setq first (ddl:pick-first-title-block))
  (if first
    (progn
      (setq attrs (ddl:get-attributes first)
            selected (ddl:choose-fields attrs))
      (if selected
        (progn
          (setq *ddl-selected-headers* (append '("STT" "LAYOUT" "BLOCK") selected '("HANDLE")))
          (setq idx 1
                rows (list (ddl:row-from-block first idx))
                idx (1+ idx))
          (princ "\nChọn thêm các block khung tên theo thứ tự cần xuất. Nhấn Enter để kết thúc.")
          (while (setq picked (entsel "\nChọn block khung tên tiếp theo: "))
    (setq ent (car picked)
          dxf (entget ent)
          type (cdr (assoc 0 dxf)))
    (cond
      ((/= type "INSERT")
        (princ "\n-> Đối tượng không phải block INSERT, bỏ qua."))
      ((null (ddl:get-attributes ent))
        (princ "\n-> Block không có attribute, bỏ qua."))
      (T
        (setq rows (append rows (list (ddl:row-from-block ent idx))))
        (princ (strcat "\n-> Đã thêm bản vẽ số " (itoa idx) "."))
        (setq idx (1+ idx)))
    )
  )
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
    (setq text (strcat (substr text 1 pos) "\"\"" (substr text (+ pos 2))))
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

(defun ddl:row-value (row header / pair)
  (setq pair (assoc header row))
  (if pair (cdr pair) "")
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
        (setq result (append result (list (ddl:row-from-block ent idx))))
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
  (setq text "")
  (foreach line lines
    (setq text (strcat text line "\r\n"))
  )
  (ddl:write-utf8 path text)
)

(defun ddl:set-attr-value (ent aliases value / obj attrs updated)
  (setq obj (vlax-ename->vla-object ent)
        attrs (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
  (if (not (vl-catch-all-error-p attrs))
    (foreach att attrs
      (if (and (not updated) (ddl:list-contains-ci (strcase (vla-get-TagString att)) aliases))
        (progn
          (vla-put-TextString att value)
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

(defun ddl:apply-record-to-block (record / handle ent header pair aliases count)
  (setq handle (cdr (assoc "HANDLE" record))
        ent (if handle (handent handle) nil)
        count 0)
  (if ent
    (progn
      (foreach pair record
        (setq header (car pair))
        (if (ddl:importable-header-p header)
          (progn
            (setq aliases (ddl:aliases-for-header header))
            (if (and aliases (ddl:set-attr-value ent aliases (cdr pair)))
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

(defun ddl:point3d (pt)
  (vlax-3d-point (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
)

(defun ddl:active-space (doc)
  (if (= (getvar "TILEMODE") 1)
    (vla-get-ModelSpace doc)
    (vla-get-PaperSpace doc)
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

(defun ddl:fill-table (table rows / headers r c row values value)
  (setq headers (ddl:headers-for-rows rows))
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list table :vlax-true))
  (setq c 0)
  (foreach value headers
    (vl-catch-all-apply 'vla-SetText (list table 0 c value))
    (setq c (1+ c))
  )
  (setq r 1)
  (foreach row rows
    (setq values (ddl:values-for-row row headers)
          c 0)
    (foreach value values
      (vl-catch-all-apply 'vla-SetText (list table r c value))
      (setq c (1+ c))
    )
    (setq r (1+ r))
  )
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list table :vlax-false))
  (vl-catch-all-apply 'vla-Update (list table))
  table
)

(defun ddl:create-table-at (rows pt / doc space headers table)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        space (ddl:active-space doc)
        headers (ddl:headers-for-rows rows)
        table (vl-catch-all-apply
                'vla-AddTable
                (list space (ddl:point3d pt) (1+ (length rows)) (length headers) 8.0 35.0)))
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
    (vl-catch-all-apply 'vla-Delete (list *ddl-last-table*))
  )
  (setq *ddl-last-table* nil)
  (if pt
    (ddl:create-table-at rows pt)
    (ddl:export-table rows)
  )
)

(defun ddl:read-table-records (table / rows cols r c headers data row value)
  (setq rows (vl-catch-all-apply 'vla-get-Rows (list table))
        cols (vl-catch-all-apply 'vla-get-Columns (list table)))
  (if (or (vl-catch-all-error-p rows) (vl-catch-all-error-p cols))
    nil
    (progn
      (setq c 0)
      (while (< c cols)
        (setq value (vl-catch-all-apply 'vla-GetText (list table 0 c)))
        (setq headers (append headers (list (if (vl-catch-all-error-p value) "" (strcase value)))))
        (setq c (1+ c))
      )
      (setq r 1)
      (while (< r rows)
        (setq c 0 row nil)
        (while (< c cols)
          (setq value (vl-catch-all-apply 'vla-GetText (list table r c)))
          (setq row (append row (list (if (vl-catch-all-error-p value) "" value)))
                c (1+ c))
        )
        (setq data (append data (list row))
              r (1+ r))
      )
      (ddl:records-from-grid headers data)
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

(defun ddl:excel-cell (sheet row col)
  (vl-catch-all-apply 'vlax-get-property (list sheet 'Cells row col))
)

(defun ddl:excel-cell-value (sheet row col / cell value)
  (setq cell (ddl:excel-cell sheet row col))
  (if (vl-catch-all-error-p cell)
    nil
    (progn
      (setq value (vl-catch-all-apply 'vlax-get-property (list cell 'Value2)))
      (if (vl-catch-all-error-p value) nil value)
    )
  )
)

(defun ddl:excel-put-cell (sheet row col value / cell result)
  (setq cell (ddl:excel-cell sheet row col))
  (if (vl-catch-all-error-p cell)
    nil
    (progn
      (setq result (vl-catch-all-apply 'vlax-put-property (list cell 'Value2 value)))
      (not (vl-catch-all-error-p result))
    )
  )
)

(defun ddl:export-xlsx (rows path / excel books book sheets sheet headers r c row header values value columns result)
  (setq excel (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))
  (if (vl-catch-all-error-p excel)
    nil
    (progn
      (vl-catch-all-apply 'vlax-put-property (list excel 'DisplayAlerts :vlax-false))
      (setq books (vl-catch-all-apply 'vlax-get-property (list excel 'Workbooks)))
      (if (vl-catch-all-error-p books)
        (progn (ddl:excel-cleanup excel nil) nil)
        (progn
          (setq book (vl-catch-all-apply 'vlax-invoke-method (list books 'Add)))
          (if (vl-catch-all-error-p book)
            (progn (ddl:excel-cleanup excel nil) nil)
            (progn
              (setq sheets (vl-catch-all-apply 'vlax-get-property (list book 'Worksheets))
                    sheet (if (vl-catch-all-error-p sheets) sheets (vl-catch-all-apply 'vlax-get-property (list sheets 'Item 1)))
                    headers (ddl:headers-for-rows rows)
                    c 1)
              (if (vl-catch-all-error-p sheet)
                (progn (ddl:excel-cleanup excel book) nil)
                (progn
                  (foreach header headers
                    (ddl:excel-put-cell sheet 1 c header)
                    (setq c (1+ c))
                  )
                  (setq r 2)
                  (foreach row rows
                    (setq values (ddl:values-for-row row headers)
                          c 1)
                    (foreach value values
                      (ddl:excel-put-cell sheet r c value)
                      (setq c (1+ c))
                    )
                    (setq r (1+ r))
                  )
                  (setq columns (vl-catch-all-apply 'vlax-get-property (list sheet 'Columns)))
                  (if (not (vl-catch-all-error-p columns))
                    (vl-catch-all-apply 'vlax-invoke-method (list columns 'AutoFit))
                  )
                  (setq result (vl-catch-all-apply 'vlax-invoke-method (list book 'SaveAs path 51)))
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

(defun ddl:read-xlsx-records (path / excel books book sheets sheet row col headers data values value done)
  (setq excel (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))
  (if (vl-catch-all-error-p excel)
    nil
    (progn
      (setq books (vl-catch-all-apply 'vlax-get-property (list excel 'Workbooks)))
      (if (vl-catch-all-error-p books)
        (progn (ddl:excel-cleanup excel nil) nil)
        (progn
          (setq book (vl-catch-all-apply 'vlax-invoke-method (list books 'Open path)))
          (if (vl-catch-all-error-p book)
            (progn (ddl:excel-cleanup excel nil) nil)
            (progn
              (setq sheets (vl-catch-all-apply 'vlax-get-property (list book 'Worksheets))
                    sheet (if (vl-catch-all-error-p sheets) sheets (vl-catch-all-apply 'vlax-get-property (list sheets 'Item 1)))
                    col 1)
              (while (and (not (vl-catch-all-error-p sheet)) (< col 200) (/= (setq value (ddl:excel-cell-value sheet 1 col)) nil))
                (setq headers (append headers (list (strcase (vl-princ-to-string value))))
                      col (1+ col))
              )
              (setq row 2)
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

(defun c:DDLXLSX ( / rows default-path path)
  (setq rows (ddl:pick-title-blocks))
  (if rows
    (progn
      (setq *ddl-last-handles* (mapcar '(lambda (row) (cdr (assoc "HANDLE" row))) rows))
      (setq default-path (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) "_DrawingList.xlsx"))
      (setq path (getfiled "Lưu Excel danh mục bản vẽ" default-path "xlsx" 1))
      (if path
        (if (ddl:export-xlsx rows path)
          (progn
            (setq *ddl-last-xlsx* path)
            (princ (strcat "\n-> Đã xuất Excel XLSX: " path))
          )
          (alert "Không xuất được XLSX. Máy có thể chưa cài Excel hoặc file đang mở.")
        )
      )
    )
  )
  (princ)
)

(defun c:DDLIMPORTXLSX ( / path records total)
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

(defun c:DDLTAGS ( / picked ent attrs pair)
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

(defun c:DDL ( / rows default-path path)
  (setq rows (ddl:pick-title-blocks))
  (if rows
    (progn
      (setq default-path (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) "_DrawingList.csv"))
      (setq path (getfiled "Lưu danh mục bản vẽ CSV" default-path "csv" 1))
      (if path
        (if (ddl:export-csv rows path)
          (progn
            (princ (strcat "\n-> Đã xuất danh mục bản vẽ: " path))
            (startapp "explorer.exe" (strcat "\"" path "\""))
          )
          (alert "Không ghi được file CSV. Hãy kiểm tra quyền ghi hoặc đường dẫn.")
        )
      )
    )
    (princ "\nKhông có block khung tên nào được chọn.")
  )
  (princ)
)

(defun c:DDLALL ( / rows default-path xlsx-path table-ok xlsx-ok)
  (setq rows (ddl:pick-title-blocks))
  (if rows
    (progn
      (setq *ddl-last-handles* (mapcar '(lambda (row) (cdr (assoc "HANDLE" row))) rows))
      (setq table-ok (ddl:export-table rows))
      (setq default-path (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) "_DrawingList.xlsx"))
      (setq xlsx-path (getfiled "Lưu Excel danh mục bản vẽ" default-path "xlsx" 1))
      (if (and xlsx-path (fboundp 'ddl:export-xlsx))
        (progn
          (setq xlsx-ok (ddl:export-xlsx rows xlsx-path))
          (if xlsx-ok (setq *ddl-last-xlsx* xlsx-path))
        )
      )
      (cond
        ((and table-ok xlsx-ok) (princ "\n-> Đã tạo CAD Table và xuất Excel XLSX."))
        (table-ok (princ "\n-> Đã tạo CAD Table. Excel chưa xuất hoặc bị lỗi."))
        (xlsx-ok (princ "\n-> Đã xuất Excel XLSX. CAD Table chưa tạo hoặc bị lỗi."))
        (T (alert "Không tạo được Table hoặc Excel. Hãy kiểm tra CAD Table API/Excel."))
      )
    )
    (princ "\nKhông có block khung tên nào được chọn.")
  )
  (princ)
)

(defun c:DDLSYNC ( / rows)
  (if *ddl-last-handles*
    (progn
      (setq rows (ddl:rows-from-handles *ddl-last-handles*))
      (if rows
        (progn
          (if (and *ddl-last-table* (fboundp 'ddl:refresh-table))
            (ddl:refresh-table rows)
          )
          (if (and *ddl-last-xlsx* (fboundp 'ddl:export-xlsx))
            (ddl:export-xlsx rows *ddl-last-xlsx*)
          )
          (princ "\n-> Đã đồng bộ lại Table/XLSX từ block khung tên.")
        )
        (alert "Không tìm thấy lại các block đã lưu handle.")
      )
    )
    (alert "Chưa có danh sách block gần nhất. Hãy chạy DDLALL hoặc DDLXLSX trước.")
  )
  (princ)
)

(defun c:DDLIMPORTTABLE ( / picked obj records total)
  (setq picked (entsel "\nChọn CAD Table danh mục để nhập ngược về khung tên: "))
  (if picked
    (progn
      (setq obj (vlax-ename->vla-object (car picked)))
      (if (= (vla-get-ObjectName obj) "AcDbTable")
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

(princ "\nDAC Drawing List loaded. Lệnh: DDL, DDLALL, DDLXLSX, DDLSYNC, DDLIMPORTTABLE, DDLIMPORTXLSX, DDLTAGS.")
(princ)
