#!/bin/bash

pg_conf_file=/var/lib/postgresql/data/postgresql.conf

echo "\
log_statement = 'all'
log_disconnections = off
log_duration = on
log_min_duration_statement = -1
shared_preload_libraries = 'pg_stat_statements'
track_activity_query_size = 2048
pg_stat_statements.track = all
pg_stat_statements.max = 10000
" >>$pg_conf_file

for database in $(echo $POSTGRES_MULTIPLE_DATABASES | tr ',' ' '); do
  echo "Creating database $database"
  psql -U $POSTGRES_USER <<-EOSQL
    CREATE DATABASE $database;
    GRANT ALL PRIVILEGES ON DATABASE $database TO $POSTGRES_USER;
    CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
EOSQL

  psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" -d "$database" <<-EOSQL
    CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
EOSQL
done
