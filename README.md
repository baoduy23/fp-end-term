# Tank Battle Arena

Đồ án cuối kỳ môn Lập trình hàm với Haskell. Game bắn xe tăng thời gian thực trên lưới 32 x 22 ô, có 3 chế độ: đấu Bot, 2 người trên một bàn phím, tối đa 3 người qua mạng LAN. Đồ họa vẽ bằng Gloss; máy chủ giữ trạng thái trong `TVar`, nhận lệnh qua TCP và trả lời dò tìm phòng qua UDP.

Báo cáo khoa học đầy đủ (định dạng 2 cột IMRAD): `Báo cáo.docx`. Nội dung slide thuyết trình (10 phút): `prompt_canvas.md`.

## 1. Môi trường

| Thành phần | Phiên bản |
| :--- | :--- |
| GHC | 9.10.x (nhóm build trên Windows với 9.10.3) |
| Cabal | 3.x |
| Thư viện ngoài | `gloss`, `network`, `stm`, `async`, `random`, `containers`, `bytestring`, `directory`, `filepath`, `process` |
| Hệ điều hành | Windows 10/11 (âm thanh dùng WinMM, tắt bộ gõ dùng imm32) |

`freeglut.dll` phải nằm cạnh `tank-client.exe`.

## 2. Biên dịch, kiểm thử, chạy

```powershell
.\build.bat
cabal --builddir="C:\cabal-build\tank-battle" test
.\tank-client.exe
```

- `build.bat` chạy `cabal build all` rồi chép `tank-client.exe`, `tank-server.exe` mới ra thư mục gốc. Exe ở thư mục gốc không tự cập nhật khi sửa code, sửa xong phải chạy lại `build.bat`.
- Bộ cờ cảnh báo: `-Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints`. Kỳ vọng 0 lỗi, 0 cảnh báo.
- `cabal test`: 130 test case trong 14 nhóm, phải ra 130 / 130, sau đó in phép đo `foldl` / `foldl'` (mục 6).
- Hai exe được biên dịch với `-threaded -with-rtsopts=-N` và khi chạy tự đặt số capability bằng số nhân CPU.

Chỉ cần mở `tank-client.exe`. Client tự mở máy chủ nhúng ở cổng TCP 9123 và UDP 9124; nếu trên máy đã có máy chủ Tank Battle đang chạy thì client dùng chung máy chủ đó. `tank-server.exe` dùng khi muốn chạy máy chủ riêng.

## 3. Luồng chơi

1. **Đăng nhập:** tên phi công 1–14 ký tự không dấu (chữ, số, `.` `_` `-` `@`) và mật khẩu. Tài khoản mới được tạo tự động, tặng 2000 vàng, lưu tại `saves/account_<tên>.dat`. Sai mật khẩu hoặc tệp hỏng thì báo lỗi ngay trên form, tệp hỏng không bị ghi đè.
2. **Sảnh:** xem tướng, vào Cửa hàng (F3) mua tướng, Sự kiện (F5) nhận thưởng theo số trận.
3. **Đấu Bot 1v1 (F1):** chọn tướng trong 30 giây, Bot chọn ngẫu nhiên một tướng khác trong kho tướng của bạn.
4. **Local 2P (F2):** màn chọn tướng 3 cột: P1 bên trái, lưới 10 tướng ở giữa, P2 bên phải. Hai người chọn cùng lúc, không được trùng tướng.
5. **Mạng LAN:**
   - Chủ phòng bấm **Tạo phòng**; màn phòng chờ hiện mã 4 số và IP LAN.
   - Người vào bấm **Dò tìm LAN** (ngay tại Sảnh chính hoặc trong màn hình Vào phòng): client mở luồng thu UDP bất đồng bộ tìm trong 0.8 giây (giao diện không bị treo) và tự động điền cả địa chỉ IP máy chủ lẫn Mã phòng 4 chữ số vào ô nhập liệu, chỉ còn bấm **Vào phòng**. Không thấy phòng thì báo "Chưa tìm thấy phòng LAN nào đang mở - Bạn có thể bấm Tạo Phòng ngay!". Vẫn tự nhập IP và mã phòng thủ công được.
   - Khách bấm `[ CHƯA SẴN SÀNG ]` / `[ ĐÃ SẴN SÀNG ]`. Chỉ chủ phòng có nút `[ BẮT ĐẦU VÀO TRẬN ]`, và chỉ bắt đầu được khi có ít nhất 2 người và mọi khách đã sẵn sàng; nếu không, cả phòng thấy thông báo ghi đúng tên người chưa sẵn sàng.
   - Chọn tướng 30 giây đồng bộ: thấy con trỏ của người khác theo thời gian thực (P1 xanh, P2 vàng, P3 tím), tướng người khác đang giữ thì không chọn được.
6. **Trận đấu 3 phút:** hết giờ, ai nhiều điểm hơn thắng. Hết trận mọi thứ đứng yên, hiện bảng kết quả (tiêu đề 2 dòng, tự co chữ cho vừa khung) và tự về sảnh sau 10 giây.
7. **Đầu hàng (F9):** hiện hộp thoại xác nhận. Đồng ý thì xe bị xóa khỏi trận, hiện "BẠN ĐÃ ĐẦU HÀNG TRẬN ĐẤU! / TỰ ĐỘNG THOÁT SAU 5 GIÂY..." rồi về sảnh. Tắt cửa sổ hoặc mất kết nối cũng tính là đầu hàng. Trận 2 người: người còn lại thắng ngay. Trận 3 người: hai người còn lại đánh tiếp.

## 4. Phím điều khiển

| Màn hình | Người chơi | Phím |
| :--- | :--- | :--- |
| Chọn tướng | Bot / LAN | `W A S D` di chuyển con trỏ, `G` hoặc `H` hoặc click để khóa |
| Chọn tướng | Local 2P | P1: `W A S D` + `G`/`H`. P2: mũi tên + `1`/`2`. Chuột: click thẻ tướng và nút xác nhận |
| Trong trận | Bot / LAN | `W A S D` di chuyển, `G` bắn, `H` tuyệt kỹ |
| Trong trận | Local 2P | P1 như trên. P2: mũi tên di chuyển, `1` bắn, `2` tuyệt kỹ |
| Trong trận | Mọi chế độ | `M` bật/tắt âm, `F9` đầu hàng (`Y` đồng ý, `N`/`Esc` hủy) |

Trong trận không dùng `Enter`, `Space` hay `Esc` (bấm `Esc` chỉ hiện nhắc nhở). Khi trận đã kết thúc, `Esc` về sảnh ngay.

**Bộ gõ tiếng Việt:** client gọi `ImmDisableIME` để tắt bộ gõ của Windows cho cửa sổ game. Unikey/EVKey không tắt được bằng cách này nên `Client/Input.hs` đổi ngược ký tự bộ gõ sinh ra về phím vật lý (ví dụ Telex bấm `w` ra `ư` thì vẫn hiểu là `W`; `aa` ra `â` sau Backspace thì hiểu là `A`). Phím chữ hoa / Caps Lock cũng được xử lý.

## 5. Luật tính điểm và cân bằng

- Hạ gục: **+150 điểm**. Bắn trúng: **+15 điểm**.
- Năng lượng tuyệt kỹ: bắn trúng **+9**, bị trúng **+5**, nhặt ngọc năng lượng **+15** (tối đa 100). Không tự hồi theo thời gian.
- Máu tính theo nửa điểm nên phần lẻ .5 của đạn thường không bị làm tròn mất.
- Sát thương tuyệt kỹ lên mỗi mục tiêu không vượt quá `min 185 (máu tối đa / 3 + 20)`, nên không tướng nào bị hạ chỉ bằng một chiêu khi đầy máu.
- Vùng trúng chiêu khớp với hiệu ứng vẽ: vòng tỏa bán kính r trúng mọi ô có tâm cách ≤ r + 0.5 ô, đúng bằng vòng tròn vẽ trên màn hình. Tia và chữ thập dừng trước tường.
- Trên bản đồ có tối đa 1 ngọc vàng (Ngôi sao bất tử hoặc Ngọc Overcharge) cùng lúc.

Bảng tướng của bản cuối: đạn thường +7.5, tuyệt kỹ +20 so với bản trước. Số liệu lấy từ `src/Hero.hs`, phần mô tả trên thẻ tướng sinh ra từ chính bản ghi này.

| Tướng | Máu | Giá | Đạn thường | Tuyệt kỹ | Hình dạng / hiệu ứng |
| :--- | ---: | ---: | ---: | ---: | :--- |
| Băng Giá (Vanguard) | 500 | 0 | 35.5 | 135 | Vòng tỏa 3 ô, đóng băng 1.8 giây |
| Bạo Chúa (Titan) | 530 | 0 | 39.5 | 150 | Đại bác bay 8 ô, đẩy lùi, phá giáp |
| Độc Xà (Viper) | 500 | 0 | 35.5 | 115 | Vòng tỏa 2 ô, hồi 130 máu, gây độc |
| Quang Năng (Solaris) | 500 | 0 | 37.5 | 155 chuẩn | Tia laze 6 ô, chói mắt |
| Hư Không (Phantom) | 500 | 0 | 37.5 | 135 | Tốc biến 4 ô (dừng trước đối thủ đầu tiên), nổ 2 ô, tàng hình |
| Hồng Ngọc (Striker) | 550 | 1500 | 39.5 | 160 | 3 làn hỏa tiễn 7 ô, chạy và bắn nhanh x2 |
| Hắc Ngọc (Aegis) | 620 | 3000 | 41.5 | 140 | Vòng tỏa 2 ô, giáp ảo +160, phản 30% sát thương |
| Lam Ngọc (Tempest) | 600 | 4500 | 39.5 | 160 chuẩn | Vòng tỏa 4 ô, EMP khóa bắn và tuyệt kỹ 2 giây |
| Thạch Anh (Mirage) | 650 | 6500 | 42.5 | 170 | Lăng kính chữ thập 3 ô, tàng hình, đạn xuyên giáp |
| Hắc Diệu (Eclipse) | 700 | 9000 | 45.5 | 180 chuẩn | Hố đen 4 ô, hút địch 2 ô, suy yếu 30% |

Bot (`AI.hs`): đủ năng lượng và chắc trúng thì dùng tuyệt kỹ, có đường ngắm thì quay nòng và bắn, bị nhắm thì né, máu thấp thì đi nhặt ngọc máu, còn lại tìm đường BFS tới ô bắn cách người chơi 3–6 ô.

## 6. Kiểm thử

`cabal test` chạy `test/Spec.hs`. Khung test tự viết, không dùng thư viện ngoài.

| Nhóm | Số test | Nội dung chính |
| :--- | ---: | :--- |
| Lưới và tọa độ | 7 | Biên bản đồ, `movePos`, khoảng cách, tia dừng trước tường |
| Type class | 9 | `Collidable`, `GameEntity`, `HasHero` nhất quán cho cả 10 tướng |
| Chiến đấu | 11 | Giáp ảo, xuyên giáp, nửa điểm máu, +15 / +150 điểm, hồi đạn, tầm bay |
| Cân bằng +7.5 / +20 | 10 | Bảng sát thương mới, mô tả trên thẻ khớp số, thời gian hạ gục 100 cặp tướng |
| Hitbox tuyệt kỹ | 13 | 10 tướng trúng ở 1 ô và ở tầm tối đa, trượt khi xa thêm 1 ô; vòng vẽ khớp `inRadius` |
| Phòng chờ và chọn tướng | 18 | Chủ phòng, sẵn sàng, chọn không trùng, đầu hàng 2 và 3 người, đóng băng sau trận |
| Parser và giao thức TCP | 11 | Khứ hồi mọi lệnh (kể cả chat tiếng Việt), vị trí lỗi, ký tự thừa |
| Dò tìm LAN (UDP) | 11 | Gói tin có IP + mã, `NONE`, dạng cũ, IP / mã sai, ưu tiên IP LAN |
| Bộ gõ và phím P1 / P2 | 8 | Telex, VIE, phím hai người không trùng |
| AI | 12 | BFS, tầm nhìn, ngắm bắn, không phí tuyệt kỹ khi địch ngoài tầm |
| Song song | 5 | `parCheckCollisions`, `parMapValues` cho kết quả bằng bản tuần tự |
| Đồng thời | 6 | `TChan`, `TVar`, `retry`, `MVar` với nhiều luồng thật |
| Tệp tài khoản | 4 | Kiểm tra tên, lưu / nạp khứ hồi, sai mật khẩu, tệp hỏng |
| Lazy evaluation | 5 | Danh sách vô hạn, tham số không dùng thì không bị tính |

Nhóm Tệp tài khoản ghi tạm vào `saves/` ở thư mục đang chạy rồi tự xóa tệp test.

Phép đo `foldl` / `foldl'` in ra sau phần test. Khi build bằng cabal (`-O1`), GHC phân tích strictness nên hai cách gần như bằng nhau (`~6 MiB` tổng cấp phát, `44 KB` Heap cực đại); khi biên dịch `-O0` với `5.000.000` phần tử, `foldl` lười tích lũy chuỗi thunk chiếm tới `312 MB` Heap cực đại và mất `1.89 s` (GC `81.9%`), trong khi `foldl'` chặt chỉ chiếm `44 KB` Heap cực đại và mất `0.16 s` (chi tiết tại Mục 4.1, Bảng 3 của `Báo cáo.docx`). Game chủ động dùng `foldl'`, trường strict `!` và `BangPatterns` để không phụ thuộc vào tối ưu hóa của compiler.

## 7. Cấu trúc mã nguồn (16 module + bộ kiểm thử)

```text
src/
  Types.hs              ADT, hằng số bản đồ, type class Collidable / GameEntity
  Hero.hs               Bản ghi HeroSpec của 10 tướng, type class HasHero, công thức sát thương
  GameLogic.hs          Luật game thuần: di chuyển, bắn, tuyệt kỹ, phòng chờ, chọn tướng, đầu hàng, par/pseq
  ParserCombinator.hs   Parser combinator tự viết (Functor / Applicative / Monad / Alternative)
  Protocol.hs           Mã hóa / giải mã lệnh, GameState và gói tin dò tìm LAN
  AI.hs                 BFS và cây quyết định của Bot
  Server/
    Launcher.hs         Mở máy chủ (dùng chung cho tank-server và máy chủ nhúng trong client)
    Network.hs          TCP, nhận theo dòng, broadcast, trả lời dò tìm LAN qua UDP
    State.hs            TVar GameState, hàng đợi lệnh TChan, thêm / xóa người chơi
    Tick.hs             Vòng lặp 30 nhịp/giây theo mốc thời gian thật
    Supervisor.hs       Giám sát luồng từng người chơi (withAsync, waitCatch, finally)
    Main.hs             Điểm vào tank-server
  Client/
    Main.hs             Vòng lặp playIO, xử lý phím / chuột, các luồng nền
    UI.hs               Vẽ toàn bộ màn hình và HUD
    Input.hs            Chuẩn hóa phím khi bật bộ gõ tiếng Việt
    Account.hs          Tài khoản, vàng, lưu / đọc tệp
    Audio.hs            Âm thanh qua WinMM / MCI
test/Spec.hs            130 test case và phép đo lazy / strict
```

## 8. Giới hạn hiện tại

- Chỉ chạy trong mạng LAN. Mỗi máy chủ có một phòng, tối đa 3 người.
- Trạng thái game gửi dạng văn bản mỗi nhịp: đủ cho LAN 3 người, không tối ưu băng thông.
- Chuẩn hóa phím tiếng Việt là heuristic: kiểu gõ VNI trong Local 2P có thể nhầm phím số của P2.
- Âm thanh và tắt bộ gõ chỉ có trên Windows.
- Song song hóa bước cập nhật xe đúng cơ chế nhưng lợi ích tốc độ nhỏ vì mỗi trận chỉ có 2–3 xe.

## 9. Phân công

| Thành viên | Module phụ trách | Nội dung chính |
| :--- | :--- | :--- |
| Lê Trần Bảo Duy | `Types.hs`, `Hero.hs`, `GameLogic.hs` | ADT, type class (`Collidable`, `GameEntity`, `HasHero`), hồ sơ 10 tướng, luật game thuần, `par` / `pseq` |
| Nguyễn Trung Tín | `ParserCombinator.hs`, `Protocol.hs`, `Server/Network.hs` | Parser combinator tự viết, giao thức đóng khung dòng TCP 9123, phản hồi & chọn phòng LAN UDP 9124 |
| Trần Minh Khang | `Server/State.hs`, `Server/Tick.hs` | STM (`TVar`, `TChan`), gom lệnh nguyên tử mỗi nhịp, vòng lặp 30 Hz theo mốc thời gian thực |
| Đặng Anh Quân | `Server/Supervisor.hs`, `Server/Launcher.hs`, `Server/Main.hs`, `AI.hs` | Giám sát luồng (`withAsync`, `waitCatch`, `finally`), máy chủ nhúng, Bot AI tìm đường BFS đệ quy |
| Trần Bảo Huy | `Client/Main.hs`, `Client/UI.hs`, `Client/Input.hs`, `Client/Account.hs`, `Client/Audio.hs`, `test/Spec.hs` | Giao diện Gloss 60 FPS, luồng dò tìm LAN tự điền IP & mã phòng, chuẩn hóa phím tiếng Việt (FFI), tệp tài khoản, âm thanh, 130 kịch bản kiểm thử |

## 10. Đối chiếu rubric

Rubric 100 điểm và bonus GUI 5 điểm theo `Guidance/YÊU CẦU PROJECT.pdf`. Bảng đối chiếu chi tiết nằm ở Mục 4.2, Bảng 4 của `Báo cáo.docx`.

| Tiêu chí | Tối đa | Minh chứng |
| :--- | ---: | :--- |
| 1. Bài toán, phạm vi và tương tác | 10 | Đăng nhập → sảnh → phòng → chọn tướng → trận → kết quả, 3 chế độ chơi |
| 2. Thiết kế hàm và xử lý trạng thái | 12 | `updateGameState`, `stepGameTick` thuần trong `GameLogic.hs`; IO ở `Server/*`, `Client/Main.hs` |
| 3. Kiểu dữ liệu, ADT và type class | 12 | `TankClass`, `RoomPhase`, `SkillShape`, `HeroSpec`, `Command`; `Collidable`, `GameEntity`, `HasHero` |
| 4. Đệ quy, hàm bậc cao và danh sách | 8 | BFS trong `AI.hs`, `foldl'` gom lệnh, danh sách vô hạn `randomStream`, list comprehension tạo bản đồ |
| 5. Monad, IO và xử lý lỗi | 8 | `Either String` khi giải mã gói tin và đăng nhập, `Maybe`, `catch` / `finally` ở mạng |
| 6. Parser combinator | 8 | `ParserCombinator.hs`; lệnh TCP, lệnh chat, gói UDP, tệp tài khoản |
| 7. Song song và đồng bộ hóa | 15 | `TVar` + `TChan` + `atomically`, luồng tick / UDP / từng client, `MVar`, `par` / `pseq`, `-N` |
| 8. Module, lưu trữ, kết nối tiến trình / mạng | 8 | 16 module phân tầng, tệp tài khoản, TCP 9123, UDP 9124, máy chủ nhúng |
| 9. Tính đúng và kiểm thử | 10 | `cabal test` 130 / 130 (14 nhóm), phép đo lazy / strict và song song |
| 10. Báo cáo và trình diễn | 5 | `Báo cáo.docx` (bài báo khoa học 2 cột IMRAD), `prompt_canvas.md` |
| 11. Đóng góp cá nhân và phản tư | 4 | Mục 9 ở trên và Mục V của `Báo cáo.docx` |
| Bonus GUI | 5 | Gloss 1280 x 720 60 FPS, chọn tướng Local 2P 3 cột, HUD đối xứng, chữ tự co không tràn khung |
