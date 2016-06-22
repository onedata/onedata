Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    Given I'm logged into Oneprovider "p1" as development user "user1"
    And I am on the /data Ember path

  Scenario: After failed upload to broken space, the same file can be successfully uploaded to correct space
    When I change the space to "Small space" with select
    And I upload "20B-0.txt" file to current dir
    And The upload of file "20B-0.txt" fails
    And I change the space to "space1" with select
    And I upload "20B-0.txt" file to current dir
    Then The upload of file "20B-0.txt" should succeed

  Scenario: After failed upload some file to broken space, an other file can be successfully uploaded to correct space
    When I change the space to "Small space" with select
    And I upload "20B-0.txt" file to current dir
    And The upload of file "20B-0.txt" fails
    And I change the space to "space1" with select
    And I upload "20B-1.txt" file to current dir
    Then The upload of file "20B-1.txt" should succeed

  Scenario: Uploading a file whose size exceeds the space quota should fail
    When I change the space to "Small space" with select
    And I upload "20B-0.txt" file to current dir
    Then The upload of file "20B-0.txt" should fail

  Scenario: Uploading a file to space should succeed
    When I change the space to "space1" with select
    And I upload "20B-0.txt" file to current dir
    Then The upload of file "20B-0.txt" should succeed

