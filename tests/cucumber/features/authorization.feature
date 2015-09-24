Feature: Authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
    Given u1 mounts onedata spaces in /root/onedata using token
    Then last operation succeeds
    Then [s1, s2] are mounted

  Scenario: Bad authorization
    Given u1 mounts onedata spaces in /root/onedata using bad token
    Then last operation fails
