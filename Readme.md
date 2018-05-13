# Clea

## About

Clea is an umbrella name for a user management and  an accounting modules tailored to specific needs.

## REST API

The enitites exposed are *token*, *user* and *book*

**host** is 159.65.188.251  
**port** is 8080  

The system comes with a predefined user with the following username and password hash:  
**username** : *admin*   
**password hash** :  *C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC*

## Bots

Currently thwo bots are supported: *alpinist* and *cleversniper*

## Roles

Currently three roles are supported: *admin*, *manager* and *client*

## *token*

The lifespan of a token is set to 24 hours

### Get a token

The example below illustrates the way to obtain a token (login)

```
curl -X POST \
  http://<host>:<port>/token \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
	"username": <username>,
	"passwordHash": <password-hash>
}'
```

### Dispose of a token

The example below illustrates the way to dispose of a token (logout)

```
curl -X DELETE \
  http://<host>:<port>/token \
  -H 'Authorization:<current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' s
```

*current-jwt-token* is the current token in use. Once caled this endpoint the token will be blacklisted

## *user*

### Get users

The example below illustrates the way of querying over all users
This endpoint has two optional query params:  

**userIds** which is a comma separated list of usernames  
**region** which is the region the user is from  

```
curl -X GET \
  'http://<host>:<port>/users?userIds=id1,id2,id3&region=arm' \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' 
```

**NOTE:** Based on the role of the requester the *region* and the *userIds* will be limited

### Create a user

The example below illustrates the way to create a user

```
curl -X POST \
  http://<host>:<port>/users \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
	"name" : "Rick",
	"surname" : "Sanchez",
	"username" : "rick_c18",
	"email" : "rickc18@citadel.com",
	"phone" : "+37493939393",
	"region" : "ukr",
	"role" : "client",
	"passwordHash" : "4DFF4EA340F0A823F15D3F4F01AB62EAE0E5DA579CCB851F8DB9DFE84C58B2B37B89903A740E1EE172DA793A6E79D560E5F7F9BD058A12A280433ED6FA46510A",
	"botContracts": [{
		"botName": "alpinist",
		"profitMargin": 0.4
	}]
}'
```

### Get own user info
The example below illustrates the way to query own data

```
curl -X GET \
  http://<host>:<port>/users/me \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json'
```


### Get a specific user

The example below illustrates the way to query some users data. In this example the users username is captured in **username-of-interest** path variable

```
curl -X GET \
  http://<host>:<port>/users/<username-of-interest> \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json'
```

### Delete a specific user

The example below illustrates the way to remove some user. In this example the users username is captured in **username-of-interest** path variable

```
curl -X DELETE \
  http://<host>:<port>/users/<username-of-interest> \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json'
```

### Edit a specific user

The example below illustrates the way to remove update a users data. In this example the users username is captured in **username-of-interest** path variable and the updated fields are captured in the body of request

```
curl -X PATCH \
  http://<host>:<port>/users/<username-of-interest> \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
	"email" : "rick_c17@citadel.com",
    "phone" : "+37491111112111"
}'
```

### Add a contract to client

The example below shows how to add a bot contract to a user. In this example the users username is captured in **username-of-interest** path variable and the body represents the contract to be added

```
curl -X POST \
  http://<host>:<port>/users/<username>/contracts \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
		"botName": "cleversniper",
		"profitMargin": 0.6
}'
```

### Request withdrawal

### Request deposit

### Get books

### Get a specific book

###

# NOTE
Also the [Postman collection](https://www.getpostman.com/collections/536663a9b343d33250e8) can be handy in testing the endpoints

