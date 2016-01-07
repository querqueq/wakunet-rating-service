## GET /ratings


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### GET Parameters:

- contents
     - **Values**: *1:post, 3:event, 5:post*
     - **Description**: List of content keys, e.g. ratings?contents[]=1:post&contents[]=3:event&contents[]=5:post
     - This parameter is a **list**. All GET parameters with the name contents[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"dislikes":1,"userId":2,"likes":2,"userRating":"Dislike","contentKey":{"contentId":13,"contentType":"post"}},{"dislikes":0,"userId":2,"likes":0,"userRating":null,"contentKey":{"contentId":3,"contentType":"event"}},{"dislikes":0,"userId":2,"likes":0,"userRating":null,"contentKey":{"contentId":5,"contentType":"post"}}]
```

## DELETE /ratings/:contentType/:contentId

#### Captures:

- *contentType*: (string) super type of rated content
- *contentId*: (long) id if rated content


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /ratings/:contentType/:contentId

#### Captures:

- *contentType*: (string) super type of rated content
- *contentId*: (long) id if rated content


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"dislikes":1,"userId":2,"likes":2,"userRating":"Dislike","contentKey":{"contentId":13,"contentType":"post"}}
```

## POST /ratings/:contentType/:contentId/dislike

#### Captures:

- *contentType*: (string) super type of rated content
- *contentId*: (long) id if rated content


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /ratings/:contentType/:contentId/like

#### Captures:

- *contentType*: (string) super type of rated content
- *contentId*: (long) id if rated content


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

