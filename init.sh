#!/bin/bash

erl -sname perforator_ci -noshell -pa ebin deps/*/ebin -config perforator_ci.config -s perforator_ci init -s init stop
