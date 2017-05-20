#!/bin/sh

stack build --profile
stack exec -- breadthw-exe -z "$@" +RTS -s > /dev/null
stack exec -- breadthw-exe "$@" +RTS -s > /dev/null
