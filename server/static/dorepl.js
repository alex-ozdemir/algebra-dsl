MathJax.Hub.Config({
    showMathMenu: false,
    ShowMathMenuMSIE: false,
    showProcessingMessages: false,
    messagStyle: "none",
    //menuSettings: {
    //    inTabOrder: false,
    //},
    //"HTML-CSS": {
    //    EqnChunkDelay: 0,
    //},
    //ignoreMMLattributes: {
    //    'onclick': false,
    //},
});

window.onload = function() {
    createCM();
}

var currentCM = null;
function createCM() {
    currentCM = CodeMirror(document.getElementById('repl'), {
        autofocus: true,
        extraKeys: {
            'Enter': sendToServer,
        },
    });
    //myCodeMirror.setSize(null, 1);

    currentCM.cmIndex = 0;

    currentCM.on('beforeSelectionChange', function(instance) {
        if (curSelected !== null) {
            curSelected.removeAttribute('selected');

            // Put a span inside that has a highlight color and has all of
            // curSelected's children
            var newspan = document.createElement('span');
            newspan.setAttribute('highlighted', currentCM.cmIndex);

            while (curSelected.children.length > 0) {
                // This (re)moves the child from curSelected
                newspan.appendChild(curSelected.children[0]);
            }

            curSelected.appendChild(newspan);

            currentCM.cmIndex += 1;
            curSelected = null;
            lastClickedID = null;
        }
    });
}

var socket = new WebSocket("ws://127.0.0.1:2794", "rust-websocket");

function sendToServer(cmBox) {
    currentCM.setOption('readOnly', true);
    console.log("sending box, value is " + cmBox.getValue());
    socket.send(cmBox.getValue());
}

socket.onmessage = function (event) {
    var data = event.data;

    var atIdx = data.indexOf('@');
    if (atIdx === -1) {
        console.log("Server sent bad data: No @ symbol");
        return;
    }
    var formulaNum = data.substr(0, atIdx);
    var rest = data.substr(atIdx+1);

    var atIdx = rest.indexOf('@');
    if (atIdx === -1) {
        console.log("Server sent bad data: Only one @ symbol");
        return;
    }
    var type = rest.substr(0, atIdx);
    var rest = rest.substr(atIdx+1);

    var fullDiv = document.createElement('div');
    fullDiv.className = 'output';

    var checkbox = document.createElement('input');
    checkbox.setAttribute('type', 'checkbox');

    fullDiv.appendChild(checkbox);

    var eqnDiv = document.createElement('div');
    eqnDiv.id = 'formula'+formulaNum;
    eqnDiv.className += ' output';
    eqnDiv.innerHTML = rest;

    fullDiv.appendChild(eqnDiv);
    document.getElementById('repl').appendChild(fullDiv);

    if (type === 'Math') {
        eqnDiv.className += ' disable-highlight';
        // Handle Actual Formula
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, eqnDiv.id]);
        MathJax.Hub.Queue([onFinishTypesetting, eqnDiv.id]);
    } else if (type === 'LaTeX') {
        checkbox.disabled = true;
        // moo
    } else if (type === 'Err') {
        eqnDiv.className += ' algebra-dsl-error';
        checkbox.disabled = true;
    }

    createCM();
};

function insertIntoCurrrentEquation(e) {

    var copyOfElement = e.cloneNode(true);

    copyOfElement.setAttribute('highlighted', currentCM.cmIndex);

    copyOfElement.removeAttribute('selected');
    var subTreeNodes = copyOfElement.getElementsByTagName("*");
    for (var i=0; i<subTreeNodes.length; i++) {
        subTreeNodes[i].removeAttribute('selected');
        subTreeNodes[i].removeAttribute('highlighted');
        subTreeNodes[i].removeAttribute('id');
    }

    var toInsert = document.createElement('span');
    toInsert.className = 'mjx-math mjx-chtml mathInEquation';
    toInsert.appendChild(copyOfElement);

    toInsert.addEventListener("mouseenter", function(ev) {
        copyOfElement.setAttribute('hoverednode',
            copyOfElement.getAttribute('highlighted'));
        e.setAttribute('hoverednode',
            copyOfElement.getAttribute('highlighted'));
    });

    toInsert.addEventListener("mouseleave", function(ev) {
        e.removeAttribute('hoverednode');
        copyOfElement.removeAttribute('hoverednode');
    });

    var literaltext = '#(mtn:' + copyOfElement.getAttribute('mathtreenode') + ')';

    if (!currentCM.somethingSelected()) {
        var place = currentCM.getCursor();

        currentCM.replaceRange(literaltext, place);

        var place2 = currentCM.getCursor();
        var x = currentCM.markText(place, place2, { replacedWith: toInsert });
        currentCM.setSelection(place, place2);
    } else {
        var oldCurSelected = curSelected;
        curSelected = null;
        currentCM.replaceSelection(literaltext, 'around');
        curSelected = oldCurSelected;

        var selections = currentCM.listSelections();
        var anchor = selections[0].anchor;
        var head = selections[0].head;
        var beg;
        var end;
        if (anchor.line < head.line || anchor.line == head.line && anchor.ch < head.ch) {
            beg = anchor;
            end = head;
        } else {
            beg = head;
            end = anchor;
        }
        currentCM.markText(beg, end, { replacedWith: toInsert });
    }
    currentCM.focus();
}

function removeSelection() {
    if (currentCM.somethingSelected()) {
        var oldCurSelected = curSelected;
        curSelected = null;
        currentCM.replaceSelection('');
        curSelected = oldCurSelected;
    }
}

var lastClickedID = null;
var curSelected = null;

function mathTreeNodeNodeAbove(cur, topLevel) {
    while (!cur.hasAttribute('mathtreenode')) {
        if (cur === topLevel) {
            return null;
        }
        cur = cur.parentNode;
    }
    return cur;
}

function onFinishTypesetting(where) {
    var element = document.getElementById(where);

    // Remove any leftover MathML
    var maths = element.getElementsByTagName('math');
    while(maths.length > 0) {
        maths.item(0).remove();
    }


    element.addEventListener("click", function(event) {
        // Remove the other highlighted stuff
        if (curSelected !== null) {
            curSelected.removeAttribute('selected');
        }

        var reset = false;
        var closestAbove = mathTreeNodeNodeAbove(event.target, element);
        if (closestAbove === null) {
            reset = true;
        } else {
            var elementToHighlight = null;
            if (closestAbove.id === lastClickedID) {
                elementToHighlight = mathTreeNodeNodeAbove(
                    curSelected.parentNode,
                    element);
            } else {
                elementToHighlight = closestAbove;
            }
            if (elementToHighlight === null || elementToHighlight.tagName === 'math') {
                reset = true;
            } else {
                elementToHighlight.setAttribute('selected', '0');

                insertIntoCurrrentEquation(elementToHighlight);
                lastClickedID = closestAbove.id;
                curSelected = elementToHighlight;
            }
        }
        if (reset) {
            removeSelection();

            lastClickedID = null;
            curSelected = null;
        }
    }, false);

}

function sendOutputLatex() {

    var tosend = "output ";

    var cns = document.getElementById('repl').childNodes;
    var first = true;
    for (var i=0; i<cns.length; i++) {
        if (cns[i].className == 'output') {
            var checkbox = cns[i].childNodes[0];
            if (checkbox.checked) {
                if (!first) {
                    tosend += ', ';
                }
                tosend += '' + Math.floor(i/2);
                first = false;
            }
        }
    }

    console.log("In output latex, sending " + tosend);
    socket.send(tosend);
}
