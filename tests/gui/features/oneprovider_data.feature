Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    Given I'm logged into Oneprovider "p1" as development user "user1"
    And I go to the /#/data relative URL

  Scenario: After failed upload to broken space, file can be successfully uploaded to correct space
    When I change the space to "space_broken"
    And The root dir of "space_broken" has no write permissions
    And I try to upload file to root dir of current space
    And The upload fails
    And I change the space to "space1"
    And I try to upload file to root dir of current space
    Then The upload should succeed
