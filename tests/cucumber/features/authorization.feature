@other_env
Feature: Authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
    Given u1 mounts onedata spaces in /root/onedata using token
    Given spaces are mounted
    Then last operation succeeds
    And mounting of [s1, s2] succeeds

  Scenario: Bad authorization
    Given u1 mounts onedata spaces in /root/onedata using bad token
    Given spaces are mounted
    Then last operation fails
