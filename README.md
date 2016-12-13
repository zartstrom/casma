

Use case:
Let's say we have a nested structure of case classes that represent a json schema. I.e. there is a REST endpoint in a finch application, that parses the body of an incoming POST-request.
Now we want to have an up-to-date documentation, what kind of data the rest endpoint accepts. That is what casma is for.

Got some ideas from https://github.com/coursera/autoschema, but wasn't entirely satisfied with it.

Uses circe as json library.


This two dependent case classes

```scala
case class Breakfast(time: String, food: List[Food])
case class Food(name: String, calories: Int)
```

get converted to the following json schema:

```javascript
{
  "type" : "object",
  "properties" : {
    "food" : {
      "type" : "array",
      "items" : {
        "type" : "object",
        "properties" : {
          "calories" : {
            "type" : "integer"
          },
          "name" : {
            "type" : "string"
          }
        }
      }
    },
    "time" : {
      "type" : "string"
    }
  }
}
```
