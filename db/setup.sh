#!/bin/sh

set -e

echo "Wait for Postgres."
until PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -c "\q"; do
    sleep 1
done
echo "Done."

echo "Create database if it doesn't exist yet."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -tc \
    "SELECT 1 FROM pg_database WHERE datname = '$DB_NAME';" | grep -q 1 \
    || PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -c "CREATE DATABASE $DB_NAME;"
echo "Done."

echo "Create tables if they don't exist yet."
PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -U $DB_USER -d $DB_NAME -f setup.sql
echo "Done."

echo "Run $@"
exec $@
