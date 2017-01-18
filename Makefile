.PHONY: server

server:
	elm-live --before-build=./cpstatic.sh --dir=public -- \
		src/Main.elm --output=public/elm.js
