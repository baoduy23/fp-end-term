@echo off
setlocal
cabal build all --builddir=C:\cabal-build\tank-battle
if errorlevel 1 (
    echo Build loi, xem thong bao o tren.
    exit /b 1
)
for %%e in (tank-client tank-server) do (
    for /f "delims=" %%p in ('cabal list-bin --builddir=C:\cabal-build\tank-battle %%e') do copy /Y "%%p" "%%e.exe" >nul
)
echo Build xong, da chep tank-client.exe va tank-server.exe ra thu muc nay.
endlocal
