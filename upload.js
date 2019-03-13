let process = require('child_process')
let clc = require('cli-color');

function system(command) {
  return new Promise(function(resolve, reject) {
      process.exec(command, function(err, stdout, stderr) {
          if (err !== null) {
              console.log('exec error: ' + err);
          }
          resolve();
      });
  });
}

async function main () {
  console.log(clc.red('start update...\nstart building...'));
  // await system("yarn build");
  console.log(clc.green('build successfully.'))
  
  console.log(clc.red('start zipping...'))
  await system('7z a ./yzmall.zip ./dist/*');
  console.log(clc.green('zip successfully.'))

  console.log(clc.red('start uploading...'))
  await system('scp ./yzmall.zip root@192.168.0.138:/yt/mgxy-server-kit/apache-tomcat-8.0.24/webapps/yzmall.war')
  console.log(clc.green('upload successfully.\nupdate complete!'))
}

main ();