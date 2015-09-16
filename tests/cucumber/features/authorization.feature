@other_env
Feature: Authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
    Given u1 mounts onedata spaces using token
    Then last operation succeeds
    And s1 are mounted

  Scenario: Bad authorization
    Given user mounts onedata spaces using bad token
    Then last operation fails
