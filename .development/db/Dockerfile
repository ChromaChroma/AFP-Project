FROM postgres:16-alpine3.18

# Copy SQL for setting up words table and importing words
COPY scheme.sql /data/scheme.sql

# Copy bash scripts to run when building
COPY create_users.sh /docker-entrypoint-initdb.d/01-create_users.sh
COPY create_dbs.sh /docker-entrypoint-initdb.d/02-create_dbs.sh
COPY import_dummy_data.sh /docker-entrypoint-initdb.d/03-import_dummy_data.sh

# Guarantee Linux file endings
RUN sed -i -e 's/\r$//' /docker-entrypoint-initdb.d/01-create_users.sh && \
    sed -i -e 's/\r$//' /docker-entrypoint-initdb.d/02-create_dbs.sh && \
    sed -i -e 's/\r$//' /docker-entrypoint-initdb.d/03-import_dummy_data.sh 
