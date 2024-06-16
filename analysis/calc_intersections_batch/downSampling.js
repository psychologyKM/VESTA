const { json } = require("express/lib/response");

function downSampling(ary){
    // [d, aa, pa, x_target, y_target, z_target, ms]
    let newAry = []
    newAry.push(ary[0]);
    currentLine = 0;
    lastMs = parseFloat(ary[currentLine][6]);
    let howManySeconds;
    while (true){
        console.log("cl", currentLine, lastMs, howManySeconds);
        while (ary[currentLine][6]<(lastMs+1000)){            
            currentLine += 1;
            
            if (currentLine >= ary.length){
                return newAry;
            }
        }

        howManySeconds = Math.floor((parseFloat(ary[currentLine][6])-lastMs)/1000);
        for (let i=1; i<howManySeconds; i++){
            let tmp = newAry[newAry.length-1];
            newAry.push(tmp);
        }
        let tmp = ary[currentLine];
        tmp[6] = parseFloat(tmp[6]%1000000)
        newAry.push(tmp);
        console.log("here:", currentLine, ary[currentLine]);
        console.log(newAry.length)
        lastMs += howManySeconds*1000;
        currentLine += 1;
        if (currentLine >= ary.length){
            return newAry;
        }
    }
}

function jsonPost(ary){
    var URL = "https://script.google.com/macros/s/AKfycbwJW0z1aQQDrf_R0nMFouqS2QknCJ9PapY7fBw9r2u2IXm0Nh4n_5OQzXasgt9f50dhyQ/exec";
    var postparam = 
        {
            "method"     : "POST",
            "mode"       : "no-cors",
            "Content-Type" : "application/json",
            "body" : JSON.stringify(ary)
        };

    fetch(URL, postparam);

}
