#!/bin/sh

stack build --profile
stack exec -- breadthw -z "$@" +RTS -s > /dev/null
stack exec -- breadthw "$@" +RTS -s > /dev/null
