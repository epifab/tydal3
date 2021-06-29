CREATE TYPE genre as ENUM('Rock', 'Pop', 'Metal');
CREATE TYPE currency as ENUM('GBP', 'USD', 'EUR');

create table artist(
  id uuid primary key,
  name varchar(128) not null,
  genres genre[] not null
);

create table venue(
  id uuid primary key,
  name varchar(128) not null,
  address text null
);

create table concert(
  id uuid primary key,
  venue_id uuid references venue(id) not null,
  begins_at timestamp without time zone not null,
  ends_at timestamp without time zone not null
);

create table ticket(
  id uuid primary key,
  concert_id uuid references concert(id) not null,
  price numeric not null,
  currency currency not null
);

create table concert_artist(
  concert_id uuid not null references concert(id),
  artist_id uuid not null references artist(id),
  index int not null,
  headliner bool not null,
  primary key (concert_id, index),
  unique (concert_id, artist_id)
);
