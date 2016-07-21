Feature: Authorization

  Background:
    Given environment is up

  Scenario: Start oneclient
    Given u1 starts oneclient in /home/u1/onedata using token
    Then last operation by u1 succeeds
