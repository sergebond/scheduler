#!/bin/sh
exec erl -pa ebin/ deps/*/ebin -s scheduler

