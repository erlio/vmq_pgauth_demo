# VerneMQ Demo Postgres Auth Plugin

This is a VerneMQ plugin demonstrating how to use the PostgreSQL DB for authenticating and authorizing clients. This is for demonstration purposes only, as it currently doesn't deal with neither SQL injection, hashing of passwords caching nor other issues related to production use.

### Prerequisites:

A recent version of Erlang OTP (e.g. 17.5, same version as VerneMQ is recommended) and obviously a running Postgres instance. Make sure to create the proper DB schema in advance, look at the ``vernemq_db.sql`` file.

### How to configure:

Change the database related configuration in src/vmq_pgauth_demo.app.src

    {hostname, "127.0.0.1"}
    {database, "vernemq_db"}
    {username, "vernemq"}
    {password, "vernemq"}

Important: after every change to this file you must recompile the plugin.

### How to (re-)compile:

    ./rebar3 compile

### How to load

    vmq-admin plugin enable -n vmq_pgauth_demo --path <path_to_vmq_pgauth_demo>/_build/default/lib

### How to unload

    vmq-admin plugin disable -n vmq_pgauth_demo

### Remarks

If you're running a vanilla VerneMQ instance you might want to disable the ``vmq_acl`` and ``vmq_passwd`` plugins.


