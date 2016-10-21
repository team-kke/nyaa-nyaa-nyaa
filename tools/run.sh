#!/bin/bash

LINE_TOKEN="your-token"

function _runWithReportingError {
    tee log < pipe &
    if $1 &> pipe; then
        notify "exited with exit code 0"
    else
        notify "`cat log | tail -n 5`"
    fi
}

function notify {
    curl -H "Authorization: Bearer $LINE_TOKEN" --data "message=$1" -X POST https://notify-api.line.me/api/notify
}

function run {
    mkfifo pipe && (_runWithReportingError "$1"; rm pipe)
}

run "nyaa $1"
