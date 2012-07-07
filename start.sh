#!/bin/bash

erl -sname perforator_www -pa ebin deps/*/ebin -config perforator_www.config -s perforator_www
