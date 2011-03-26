#!/bin/sh

erl \
    -name eds@localhost \
    -pa ../emongo/ebin \
    -pa ebin \
    -boot start_sasl \
    -eval "application:start(eds)"
