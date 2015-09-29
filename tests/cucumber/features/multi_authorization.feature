Feature: Authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
    Given [u1, u2] start oneclient nodes [client1, client1] in
      [/root/onedata1, /root/onedata2] on computers [1, 2] respectively,
      using [token, token]
    Then last operation by u1 succeeds
    And last operation by u2 succeeds
    And [s1, s2] are mounted for u1
    And [s1, s2] are mounted for u2

  Scenario: Bad and good authorization
    Given [u1, u2] start oneclient nodes [client1, client1] in
      [/root/onedata1, /root/onedata2] on computers [1, 2] respectively,
      using [bad token, token]
    Then last operation by u1 fails
    And last operation by u2 succeeds
    And [s1, s2] are mounted for u2

   Scenario: Bad authorization
    Given [u1, u2] start oneclient nodes [client1, client1] in
      [/root/onedata1, /root/onedata2] on computers [1, 2] respectively,
      using [bad token, bad token]
    Then last operation by u1 fails
    And last operation by u2 fails