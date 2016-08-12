Feature: Authorization

  Background:
    Given environment is up

#  TODO implement step for creatign nested structure of directories

  Scenario: Start oneclient
    Given u1 starts oneclient in /home/u1/onedata using token
    When profiling is started on worker1.p1
    When profiling is stopped on worker1.p1
    Then u1 remounts oneclient
    Then last operation by u1 succeeds

