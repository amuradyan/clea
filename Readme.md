# Clea

## About

Clea is an umbrella name for a user management and  an accounting modules tailored to specific needs.

## REST API

The enitites exposed are *token*, *user* and *book*

**host** is the URL of the host
**port** is the port listening for requests from Clea

The system comes with a predefined user with the following username and password hash:  
**username** : *admin*   
**password** : *admin*

Front-end **NEVER** send plain passwords to back. It should hash the password with SHA512

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

The example below shows how to issue a withdrawal. It contains two notable components, the *by* of the deposit and the *subject* of the withdrawal. In this example the *subject* is captured in **username-of-interest** path variable, the *by* is dedued from the token and the body represents the withdrawal data. **transaction-type** can be either deposit or withdraw

```
curl -X PATCH \
  http://<host>:<port>/users/<username-of-interest>/books/<book-name> \
  -H 'Authorization: <current-jwt-token> \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/json' \
  -d '{
	"type": <transaction-type>,
	"bookId": <book-name>,
	"source": <username-of-interest>,
	"amount": 100,
	"fee":100,
	"note": "edrftyuio"
}'
```

### Get books

### Get a specific book

###

# NOTE
Also the [Postman collection](https://www.getpostman.com/collections/536663a9b343d33250e8) can be handy in testing the endpoints

