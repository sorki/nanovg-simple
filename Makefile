build:
	stack build

hot:
	stack build --fast --file-watch

run:
	stack exec -- nanovg-playground

run-ultra:
	__GL_SYNC_TO_VBLANK=0 vblank_mode=0 stack exec --RTS -- nanovg-playground +RTS -s
