#!/bin/sh

erl \
    -name eds@localhost \
    -pa ../emongo/ebin \
    -pa ebin \
    -boot start_sasl \
    -eval "appmon:start(), application:start(eds)"

