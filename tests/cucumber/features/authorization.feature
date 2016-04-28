Feature: Authorization

  Background:
    Given environment is up

  Scenario: Successful authorization
    Given u1 starts oneclient in /home/u1/onedata using token
    Then last operation by u1 succeeds
    And [s1, s2] are mounted for u1

  Scenario: Bad authorization
    Given u1 starts oneclient in /home/u1/onedata using bad token
    Then last operation by u1 fails
