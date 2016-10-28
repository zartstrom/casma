

Use case:
Let's say we have a nested structure of case classes that represent a json schema. I.e. there is a REST endpoint in a finch application, that parses the body of an incoming POST-request.
Now we want to have an up-to-date documentation, what kind of data the rest endpoint accepts. That is what casma is for.

Got some ideas from https://github.com/coursera/autoschema, but wasn't entirely satisfied with it.

Uses circe as json library.
