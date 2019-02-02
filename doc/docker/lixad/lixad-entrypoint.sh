#!/bin/bash
# switch to unprivileged user
set -e
export PATH=/opt/lixa/sbin:$PATH
exec "$@"
