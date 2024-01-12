buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  doKubeDeploy = true
  buildNode = 'jenkins-agent-java17'

  doDocker = {
    buildDocker {
      publishMaster = 'no'
      healthChk = 'no'
    }
  }
}
