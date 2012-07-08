#!/bin/bash

erl -sname perforator_ci -setcookie omg -pa ebin deps/*/ebin -config perforator_ci.config -s perforator_ci
