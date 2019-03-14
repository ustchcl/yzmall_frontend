let target = 
`
id: number;
accountId: number;
name: string;
address: string;
phone: string;
`

function main () {
  let arr = target.split(";");
  let result = []
  arr.forEach(ele => {
    let index = ele.indexOf(":");
    if (index == -1) {
      return;
    } else {
      let key = ele.slice(1, index);
      result.push(key);
      console.log(`${key} <- obj .: "${key}"`)
    }
  });
  console.log(`pure { ${result.join(', ')} }`)
}

main();