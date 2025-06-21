-- https://docs.omnigres.org/omni_seq/id/#migration-guide
-- https://docs.omnigres.org/omni_id/identity_type/
-- https://yrashk.com/blog/2023/04/09/sum-types-in-postgres/
-- https://docs.omnigres.org/omni_types/sum_types/

CREATE EXTENSION IF NOT EXISTS omni_id;
CREATE EXTENSION IF NOT EXISTS omni_seq;
CREATE EXTENSION IF NOT EXISTS omni_types;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- TYPES

CREATE TYPE character.MOVEMENT_TYPE AS ENUM(
    'idle',
    'walking',
    'jumping',
    'flying'
);

CREATE TYPE character.ACTION_TYPE AS ENUM(
    'casting_spell',
    'collecting_resource'
);
