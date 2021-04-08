#!/usr/bin/bash

alex  syntax/Lexer.x  -o src/Lexer.hs  && \
happy syntax/Parser.y -o src/Parser.hs && \
stack install