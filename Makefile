build:
	stack build

hot:
	stack build --fast --file-watch

run:
	stack exec -- nanovg-playground
