Feature: Onezone spaces
  Basic operations on spaces in onezone

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And opened browser with user1 logged to "z1 onezone" service


  Scenario Outline: User creates space using <client1> and using <client2> he sees that it has been created
    When using <client1>, user1 creates space "helloworld" in "z1" Onezone service
    Then using <client2>, user1 sees that space named "helloworld" has appeared in "z1" Onezone service

    Examples:
      | client1         | client2         |
      | Onezone web GUI | REST            |
      | REST            | Onezone web GUI |
