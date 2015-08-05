-- Database: vernemq_db

-- DROP DATABASE vernemq_db;

CREATE DATABASE vernemq_db
  WITH OWNER = vernemq
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'en_US.UTF-8'
       LC_CTYPE = 'en_US.UTF-8'
       CONNECTION LIMIT = -1;

-- Table: on_register

-- DROP TABLE on_register;

CREATE TABLE on_register
(
  mountpoint character varying(128) NOT NULL,
  clientid character varying(255) NOT NULL,
  username character varying(255) NOT NULL,
  password character varying(255),
  id serial NOT NULL,
  CONSTRAINT on_register_pk PRIMARY KEY (id),
  CONSTRAINT on_register_unique UNIQUE (mountpoint, clientid, username)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE on_register
  OWNER TO vernemq;


-- Table: on_publish

-- DROP TABLE on_publish;

CREATE TABLE on_publish
(
  id serial NOT NULL,
  subscriber_id serial NOT NULL,
  qos smallint,
  topic character varying(255),
  maxpayloadsize integer,
  allow_retain boolean,
  CONSTRAINT on_publish_pk PRIMARY KEY (id),
  CONSTRAINT on_publish_fk FOREIGN KEY (subscriber_id)
      REFERENCES on_register (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (
  OIDS=FALSE
);
ALTER TABLE on_publish
  OWNER TO vernemq;

-- Table: on_subscribe

-- DROP TABLE on_subscribe;

CREATE TABLE on_subscribe
(
  id serial NOT NULL,
  topic character varying(255),
  qos smallint,
  subscriber_id serial NOT NULL,
  CONSTRAINT on_subscribe_pk PRIMARY KEY (id),
  CONSTRAINT on_subscribe_fk FOREIGN KEY (subscriber_id)
      REFERENCES on_register (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (
  OIDS=FALSE
);
ALTER TABLE on_subscribe
  OWNER TO vernemq;

