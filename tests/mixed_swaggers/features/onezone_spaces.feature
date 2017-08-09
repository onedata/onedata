Feature: Onezone spaces
  Basic operations on spaces in onezone

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And logged as user1 to z1 onezone service using web GUI


  Scenario Outline: User creates space using REST API and sees in browser that it has appeared
    When using <client1>, user1 creates space "helloworld" in "z1" Onezone service
    Then using <client2>, user1 sees that space named "helloworld" has appeared in "z1" Onezone service
    Examples:
      | client2         | client1         |
      | web GUI onezone | REST            |
      | REST            | web GUI onezone |
