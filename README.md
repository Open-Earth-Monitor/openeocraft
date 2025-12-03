# OpenEOcraft
OpenEOcraft offers a robust R framework designed for the development and deployment of openEO API applications. This package simplifies the process of creating RESTful openEO web services with its user-friendly and flexible interface. Built on Plumber, OpenEOcraft prioritizes ease of use, scalability, and adaptability.

<img src="man/figures/openeocraft-architecture.png" alt="OpenEOcraft Architecture"  />
<p class="caption">


## Easy Deployment locally
If you have R installed and the relevant packages like plumber, sits, torch, jsonlite, etc, you can run the server locally. 
You first need to clone the repository via this command:

```bash
git clone https://github.com/Open-Earth-Monitor/openeocraft.git
```

then you can change to that directory

```bash
cd openeocraft
```

Run it using Rscript :

```bash
Rscript docker/server.R
```

## Easy Deployment with Docker
If you want to change the source code then this approach is recommended.
You first need to clone the repository via this command:

```bash
git clone https://github.com/Open-Earth-Monitor/openeocraft.git
```

then you can change to that directory

```bash
cd openeocraft
```

Run it :

```bash
docker-compose up
```

Run in detached mode :

```bash
docker-compose up -d
```

Shutting it down:

```bash
docker-compose down
```

Force restart  and rebuild:

```bash
docker-compose up --build --force-recreate --no-deps -d
```

If there are new changes on the images or Dockerfiles:
```bash
docker-compose build --no-cache && docker-compose up
```
