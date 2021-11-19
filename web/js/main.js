
const NFA = {
    value: {
      vocabulary: [ 1, 0 ],
      states: [ 's1', 's2' ],
      initial: 's1',
      finals: [ 's2' ],
      moves: [ 's1/0==>s1', 's1/1==>s1', 's2/0==>s1', 's2/1==>s1' ]
    },
    type: 'nfa'
  }
const SERVICE_URL = 'http://localhost:9000/converter';

const binding = (btn, input, out) => {
    
    const onclick = () => {
        let obj = {value: input.value, type: "regex"}
        fetch(SERVICE_URL, { 
                method:'post',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify(NFA)        
            })
             .then(resp => resp.json())
             .then(json => {console.log(json);  out.innerHTML += `\n árbol: ${json.tree} 
                receiving FA = {
                            id: ${json.fa.id},
                            vocabulary: ${json.fa.vocabulary},
                            states: ${json.fa.states},
                            initial: ${json.fa.initial},
                            finals: ${json.fa.finals},                                                                        
                            moves: ${json.fa.moves}
                        }\n`})
             .catch(e => out.innerHTML += `\n*** ${e} ***`)
    }
    btn.addEventListener("click", onclick)
}

const main = () => {
    let [btn, input, out] = ["btn", "input", "out"].map( e => document.getElementById(e))
    binding(btn, input, out)
}

/////////////////////////////////////////////
window.addEventListener("load", main, false);  
/////////////////////////////////////////////