
const SERVICE_URL = 'http://localhost:9000/parser';

const binding = (btn, input, out) => {
    
    const onclick = () => {
        let obj = {value: input.value, type: "regex"}
        fetch(SERVICE_URL, { 
                method:'post',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify(obj)
            })
             .then(resp => resp.json())
             .then(json => {console.log(json);  out.innerHTML += `\n árbol: ${json.tree} --> en post-orden ${json.trail}\n`})
             .catch(e => out.innerHTML += `\n*** ${e} ***`)
    }
    btn.addEventListener("click", onclick)
}

const main = () => {
    let [btn, input, out] = ["btn", "input", "out"].map( e => document.getElementById(e))
    binding(btn, input, out)
}

//////////////////////////////////////////////////
window.addEventListener("load", main, false);  
//////////////////////////////////////////////////