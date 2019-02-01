#!/bin/bash
# switch to unprivileged user
su - lixa
set -e
export PATH=/opt/lixa/sbin:$PATH
exec "$@"
