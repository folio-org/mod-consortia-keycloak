@Library('pipelines-shared-library') _

import org.folio.eureka.EurekaImage
import org.jenkinsci.plugins.workflow.libs.Library

node('jenkins-agent-java17-bigmem') {
  stage('Build Docker Image') {
    dir('mod-consortia-keycloak') {
      EurekaImage image = new EurekaImage(this)
      image.setModuleName('mod-consortia-keycloak')
      image.makeImage()
    }
  }
}

buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  buildNode = 'jenkins-agent-java17'
}
