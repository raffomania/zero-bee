build: build-elm build-tauri

build-tauri:
    npx tauri build

build-elm:
    npx parcel build index.html