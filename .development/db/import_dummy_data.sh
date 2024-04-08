#!/bin/bash
set -e

echo "Importing dummy data"

PGPASSWORD=${DB_APP_PASS} psql --username ${DB_APP_USER} ${DB_APP_NAME} --file /data/scheme.sql