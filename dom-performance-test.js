var someString = "some string"

function updateString() {
    for (var i = 0; i <= 100000; i++) {
        someString = "updated string"
    }
}

var t1 = performance.now();
updateString();
var t2 = performance.now();

console.log("It took " + (t2 - t1) + " milliseconds to update a string");

var newDiv = document.createElement("div");
var newText = document.createTextNode("some text");

newDiv.appendChild(newText);
document.body.appendChild(newDiv);

function updateDOM() {
    for (var i = 0; i <= 100000; i++) {
        newDiv.innerHTML = "update text"  
    }
}

var t3 = performance.now();
updateDOM();
var t4 = performance.now();

console.log("It took " + (t4 - t3) +  " milliseconds to update a DOM element");