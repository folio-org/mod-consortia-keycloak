@Library('pipelines-shared-library') _

import org.folio.eureka.EurekaImage
import org.jenkinsci.plugins.workflow.libs.Library

String moduleName = 'mod-consortia-keycloak'

node('jenkins-agent-java17-bigmem') {
  stage('Build Docker Image') {
    dir(moduleName) {
      EurekaImage image = new EurekaImage(this)
      image.setModuleName(moduleName)
      image.makeImage()
    }
  }
}

buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  buildNode = 'jenkins-agent-java17-bigmem'

  doDocker = {
    buildJavaDocker {
    publishMaster = true
//       healthChk = true
//       healthChkCmd = 'wget --no-verbose --tries=1 --spider http://localhost:8081/admin/health || exit 1'
    }
  }
}
