#!/usr/bin/env sh
stack test | egrep -i "[0-9]+ examples, [0-9]+ failures"
