# DAC Plotter

Lisp hỗ trợ in hàng loạt bản vẽ trong AutoCAD, EnjiCAD và ZWCAD theo khung trong Model và Layout. Bộ tool hiện có 2 nhóm chức năng:

- `DAC_Plotter.lsp`: in hàng loạt bản vẽ.
- `DAC_DrawingList.lsp`: tạo danh mục bản vẽ tự động từ block khung tên.

Nhóm in hàng loạt có 2 lệnh chính:

- `VDC`: vẽ đường chéo xác định khung in trên layer `DAC_Plotter`.
- `DP`: mở hộp thoại chọn máy in, khổ giấy, nét in, kiểu chọn khung và danh sách layout để in hàng loạt.

## Yêu cầu

- Phần mềm CAD có hỗ trợ AutoLISP/Visual LISP, ví dụ AutoCAD, EnjiCAD hoặc ZWCAD.
- Có ít nhất một máy in PDF trong phần mềm CAD đang dùng, ví dụ:
  - `AutoCAD PDF (General Documentation).pc3`
  - `DWG To PDF.pc3`
  - `PDF24`
- Nếu dùng `AutoCAD PDF` hoặc `DWG To PDF` và muốn ghép nhiều trang thành một PDF, máy cần có `qpdf.exe`.
  - Cách dễ nhất là cài PDF24, vì PDF24 thường có sẵn qpdf.
  - Lisp đang dò qpdf tại các vị trí phổ biến như `C:\Program Files\PDF24\qpdf\bin\qpdf.exe`.

## Cài đặt

1. Cài PDF24 trước khi dùng tool.
   - PDF24 cung cấp máy in PDF ổn định và thường có sẵn `qpdf.exe` để ghép nhiều trang PDF.
   - Sau khi cài PDF24, mở lại phần mềm CAD để CAD nhận máy in PDF mới.
2. Mở AutoCAD, EnjiCAD hoặc ZWCAD.
3. Gõ lệnh `APPLOAD`.
4. Chọn file `DAC_Plotter.lsp`.
5. Bấm `Load`.
6. Nếu muốn CAD tự nạp Lisp mỗi lần mở, thêm `DAC_Plotter.lsp` vào `Startup Suite` trong hộp thoại `APPLOAD` nếu phần mềm có hỗ trợ.

Sau khi load thành công, có thể dùng trực tiếp 2 lệnh `VDC` và `DP` trên command line của CAD.

Nếu cần dùng chức năng tạo danh mục bản vẽ tự động, load thêm file `DAC_DrawingList.lsp` bằng `APPLOAD`. Sau khi load thành công, command line sẽ báo các lệnh `DAL`, `DALSYNC`, `DALSYNCBACK`, `DALIMPORTTABLE`, `DALIMPORTXLSX`, `DALTAGS`.

## Cách sử dụng nhanh

### 1. Tạo khung in bằng đường chéo

1. Gõ `VDC`.
2. Bật OSNAP để bắt chính xác góc khung tên hoặc khung bản vẽ.
3. Chọn góc thứ nhất của khung.
4. Chọn góc đối diện.
5. Lặp lại cho các khung cần in.
6. Nhấn `Enter` hoặc chuột phải để kết thúc.

Lệnh `VDC` tự tạo và dùng layer `DAC_Plotter`. Layer này được đặt không in ra giấy, nên đường chéo chỉ dùng để xác định vùng in.

### 2. In hàng loạt

1. Gõ `DP`.
2. Chọn `Máy in (Printer/Plotter)`.
   - Danh sách này lấy trực tiếp từ phần mềm CAD đang chạy, nên AutoCAD, EnjiCAD và ZWCAD có thể hiển thị khác nhau.
3. Chọn `Khổ giấy (Paper size)`.
4. Chọn `Nét in (Plot style)`, ví dụ `monochrome.ctb`.
   - Danh sách này lấy từ Plot Style Table của CAD đang chạy, gồm các file `.ctb` và `.stb` nếu CAD tìm thấy.
5. Chọn kiểu lấy khung in:
   - `Theo Layer`: dùng các LINE/LWPOLYLINE trên layer đã chọn, mặc định là `DAC_Plotter`.
   - `Theo Title Block`: dùng bounding box của block khung tên đã chọn.
   - `Theo Khung tên chọn`: click trực tiếp các block khung tên theo đúng thứ tự muốn in.
6. Chọn layout cần in.
   - Mặc định chọn `All`.
   - Bấm nút `All` hoặc dùng `Ctrl+A` trong danh sách layout để chọn toàn bộ.
7. Bấm `Bắt đầu in`.

Với `Theo Layer` và `Theo Title Block`, tool sẽ sắp xếp thứ tự in theo layout, trong từng layout sẽ in từ trái sang phải và từ trên xuống dưới. Với `Theo Khung tên chọn`, tool giữ đúng thứ tự người dùng đã click.

### 3. Tự chọn thứ tự in bằng khung tên

1. Trong lệnh `DP`, chọn `Theo Khung tên chọn`.
2. Bấm `Chọn khung tên`.
3. Click từng block khung tên theo đúng thứ tự cần in.
4. Nhấn `Enter` để kết thúc chọn.
5. Lisp sẽ vẽ số thứ tự lớn màu vàng trên layer `DAC_Plotter_Order`.
6. Bấm `Reset` nếu muốn xóa số thứ tự trên bản vẽ và chọn lại từ đầu.
7. Bấm `Bắt đầu in`.

Layer `DAC_Plotter_Order` được đặt không in ra giấy, nên số thứ tự chỉ dùng để kiểm tra trong CAD và không xuất ra PDF.

## Cách tool xác định vùng in

- Với đối tượng `LINE`, tool dùng đúng 2 đầu mút đường chéo làm cửa sổ in.
- Với `LWPOLYLINE` hoặc `INSERT` block, tool dùng bounding box của đối tượng.
- Khi in theo Title Block, nếu block có đối tượng phụ, attribute hoặc hình học nằm ngoài khung tên, bounding box có thể rộng hơn khung nhìn thấy. Trường hợp cần chính xác tuyệt đối theo khung, nên dùng đường chéo `VDC` trên layer `DAC_Plotter`.
- Khi in theo Khung tên chọn, tool dùng bounding box của từng block được click và in theo đúng thứ tự click.

## Xuất PDF và ghép file

- Với `AutoCAD PDF` hoặc `DWG To PDF`, tool xuất từng trang PDF tạm vào thư mục `_DAC_Plotter_Temp`, sau đó dùng qpdf để ghép thành file:

```text
<Tên bản vẽ>_Combined.pdf
```

- Sau khi ghép thành công, tool sẽ xóa các PDF tạm và xóa thư mục `_DAC_Plotter_Temp`.
- Tool chỉ mở file PDF đã ghép cuối cùng.
- Nếu có PC3 dạng `DAC_NoViewer_*.pc3`, tool sẽ tự ưu tiên dùng PC3 này để tránh CAD tự mở PDF lẻ sau mỗi lần plot.

Khi chạy `DP`, command line sẽ báo một trong hai trạng thái:

```text
-> Dùng PC3 không mở PDF lẻ: DAC_NoViewer_...
```

hoặc:

```text
-> Cảnh báo: Chưa tìm thấy PC3 DAC_NoViewer_, PDF lẻ có thể tự mở sau khi in.
```

## Lưu ý khi chọn máy in

- `PDF24` thường ít bị viền trắng hơn với một số khổ giấy riêng.
- `AutoCAD PDF` và `DWG To PDF` phụ thuộc nhiều vào cấu hình PC3, khổ giấy và printable area.
- Nếu in trong Layout bị sai vùng so với khung tên, kiểm tra lại:
  - khung đang chọn là đường chéo `DAC_Plotter` hay Title Block;
  - block khung tên có phần tử nào nằm ngoài khung hay không;
  - khổ giấy đang chọn có đúng chiều ngang/dọc với khung không.

## Xử lý lỗi thường gặp

### Không thấy máy in

Kiểm tra CAD đang dùng đã có máy in PDF như `DWG To PDF.pc3`, `AutoCAD PDF.pc3`, `PDF24` hoặc máy in PDF tương đương chưa. Sau đó đóng và mở lại CAD nếu danh sách máy in chưa cập nhật.

### Báo không tìm thấy qpdf

Cài PDF24 hoặc qpdf độc lập, rồi chạy lại `DP`.

### PDF lẻ vẫn tự mở sau khi in

Kiểm tra command line khi chạy `DP`. Nếu không thấy dòng `Dùng PC3 không mở PDF lẻ`, CAD có thể chưa thấy file `DAC_NoViewer_*.pc3` hoặc đang load nhầm bản Lisp cũ.

### In theo Title Block bị rộng hơn khung

Nguyên nhân thường là bounding box của block rộng hơn đường khung nhìn thấy. Dùng `VDC` để vẽ đường chéo đúng 2 góc khung cần in, rồi in theo `Theo Layer` với layer `DAC_Plotter`.

## File cấu hình

Tool lưu lựa chọn gần nhất vào file cấu hình cạnh `DAC_Plotter.lsp` khi có thể. Nếu không tìm thấy đường dẫn Lisp, cấu hình sẽ được lưu trong thư mục bản vẽ hiện tại.

# DAC Drawing List

`DAC_DrawingList.lsp` dùng để trích xuất thông tin từ block khung tên và tạo danh mục bản vẽ trong CAD Table, Excel `.xlsx` hoặc CSV.

## Yêu cầu

- Phần mềm CAD có hỗ trợ AutoLISP/Visual LISP.
- Bản vẽ cần dùng block khung tên có Attribute hoặc MText chứa thông tin như số bản vẽ, tên bản vẽ, revision.
- Để xuất `.xlsx` hoặc import ngược từ Excel, máy cần cài Microsoft Excel vì Lisp dùng Excel COM.

## Cài đặt

1. Mở AutoCAD, EnjiCAD hoặc ZWCAD.
2. Gõ `APPLOAD`.
3. Chọn file `DAC_DrawingList.lsp`.
4. Bấm `Load`.
5. Nếu muốn CAD tự nạp Lisp mỗi lần mở, thêm `DAC_DrawingList.lsp` vào `Startup Suite` nếu phần mềm có hỗ trợ.

Sau khi load thành công, có thể dùng các lệnh:

- `DAL`: chọn block khung tên mẫu, cấu hình trường cần lấy và tạo danh mục.
- `DALSYNC`: đồng bộ lại CAD Table/Excel/CSV gần nhất từ block khung tên đã chọn trước đó.
- `DALSYNCBACK`: đồng bộ ngược từ CAD Table danh mục về Attribute của block khung tên.
- `DALIMPORTTABLE`: đọc CAD Table danh mục và cập nhật ngược về Attribute của block khung tên.
- `DALIMPORTXLSX`: đọc Excel danh mục và cập nhật ngược về Attribute của block khung tên.
- `DALTAGS`: chọn một block khung tên để xem nhanh danh sách tag/value.

## Cách sử dụng nhanh

### 1. Tạo danh mục bản vẽ

1. Gõ `DAL`.
2. Click chọn một block khung tên mẫu.
3. Lisp sẽ tự quét toàn bộ block cùng tên trong bản vẽ hiện tại.
4. Trong popup, chọn các Attribute/MText cần đưa vào danh mục.
5. Chọn kiểu xuất:
   - `Create Table`: tạo bảng trong CAD.
   - `Write Excel`: xuất file Excel `.xlsx`.
   - `Write CSV`: xuất file `.csv`.
   - `OK`: tạo CAD Table và xuất file Excel/CSV theo cấu hình.
6. Chọn điểm đặt bảng trong CAD nếu có tạo Table.
7. Chọn vị trí lưu file nếu có xuất Excel/CSV.

Danh sách block được sắp xếp theo thứ tự tab layout từ trái sang phải. Trong từng layout hoặc Model, các block được sắp xếp theo vị trí trên bản vẽ: từ trên xuống dưới và từ trái sang phải.

### 2. Chọn trường thông tin cần trích xuất

Trong popup cấu hình:

- `Block Name`: tên block khung tên đang dùng.
- `Tag List`: danh sách tag Attribute/MText có trong block.
- `Tag List Extract`: danh sách trường sẽ đưa vào danh mục.
- `Add`: thêm tag vào danh mục.
- `Remove`: bỏ tag khỏi danh mục.
- `Up` / `Down`: đổi thứ tự cột xuất ra.
- `Text style`: chọn style chữ có sẵn trong file CAD.
- `Text height`: chiều cao chữ cho CAD Table.
- `Second language`: chọn ngôn ngữ thứ 2 cho dòng chữ đỏ trong Excel.

## Định dạng Excel

File Excel xuất ra theo format danh mục bản vẽ:

- Cột `A` và hàng `1` được chừa trống.
- Ô `B2:E2`: tiêu đề `DANH MỤC BẢN VẼ`.
- Ô `B3:E3`: tên tiếng Anh/Trung/Nhật tùy chọn, hiển thị màu đỏ.
- Hàng `4`: header bảng.
- Từ hàng `5`: dữ liệu danh mục bản vẽ.
- Cột `B`: `STT`, số thứ tự.
- Cột `C`: `SỐ HIỆU BẢN VẼ`, số hiệu bản vẽ.
- Cột `D:E`: `TÊN BẢN VẼ`, gồm tên chính và ngôn ngữ thứ 2.

Lisp vẫn lưu `HANDLE` của block ở cột kỹ thuật `R` để phục vụ import/sync. Cột này không dùng để trình bày danh mục và có thể được ẩn nếu Excel cho phép.

## Đồng bộ và cập nhật ngược

Tool hỗ trợ đồng bộ hai chiều:

- `DALSYNC`: từ block khung tên ra CAD Table/Excel/CSV.
- `DALSYNCBACK` hoặc `DALIMPORTTABLE`: từ CAD Table về block khung tên.
- `DALIMPORTXLSX`: từ Excel về block khung tên.

### Đồng bộ từ block khung tên ra bảng/file

Sau khi đã chạy `DAL`, nếu người dùng sửa Attribute trong block khung tên:

1. Gõ `DALSYNC`.
2. Lisp sẽ đọc lại các block đã chọn trước đó.
3. CAD Table và file Excel/CSV gần nhất sẽ được cập nhật lại nếu còn truy cập được.

### Cập nhật từ Excel về khung tên

1. Mở file Excel danh mục và sửa các trường cần cập nhật.
2. Lưu và đóng file Excel.
3. Quay lại CAD, gõ `DALIMPORTXLSX`.
4. Chọn file Excel vừa sửa.
5. Lisp dùng `HANDLE` kỹ thuật để tìm đúng block và cập nhật Attribute tương ứng.

Không cần sửa cột `R`; đây là cột kỹ thuật để Lisp tìm đúng block khung tên.

### Cập nhật từ CAD Table về khung tên

1. Sửa trực tiếp nội dung trong CAD Table danh mục.
2. Gõ `DALSYNCBACK` hoặc `DALIMPORTTABLE`.
3. Click chọn CAD Table.
4. Lisp đọc bảng và cập nhật ngược về Attribute của block khung tên.

## Lưu ý

- Nếu block khung tên không có Attribute, dùng `DALTAGS` để kiểm tra nhanh dữ liệu có đọc được hay không.
- Khi import ngược từ Excel, không xóa hoặc sửa cột kỹ thuật chứa `HANDLE`, vì cột này giúp Lisp tìm đúng block cần cập nhật.
- Nếu Excel đang mở file `.xlsx`, Lisp có thể không lưu hoặc không import được. Hãy lưu và đóng file trước khi chạy lệnh trong CAD.
- Nếu cần xuất danh mục trình bày đẹp trong CAD, nên chọn đúng `Text style` có sẵn trong file trước khi tạo Table.
