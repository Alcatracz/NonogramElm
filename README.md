# Nonogramm Elm

This is an elm implementation for the puzzle game Nonogramm in Elm.
## [Demo](https://alcatracz.github.io/NonogramElm/)

## Build from source

`elm make src/Main.elm`

Add this code to the bottom of the generated html file for state persistence
```javascript
var storedData = localStorage.getItem('myapp-model');  //this
var flags = storedData ? JSON.parse(storedData) : null; //this  

var app = Elm.Main.init({ node: document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49"), flags: flags }); //this
if (document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49"))
{
  document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49").innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.';
}
app.ports.setStorage.subscribe(function (state) {             //this
	localStorage.setItem('myapp-model', JSON.stringify(state)); //this
});                                                           //this
```
