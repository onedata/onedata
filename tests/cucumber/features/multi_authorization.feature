Feature: Multi_authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]
    Then last operation by u1 succeeds
    And last operation by u2 succeeds
    And [s1, s2] are mounted for u1
    And [s1, s2] are mounted for u2

  Scenario: Successful authorization - 1 client per user
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]
    Then last operation by u1 succeeds
    And last operation by u2 succeeds
    And [s1] are mounted for u1
    And [s2] are mounted for u2

  Scenario: Successful authorization - 2 clients of one user
    Given [u1, u1] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u1/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]
    Then last operation by u1 succeeds
    And last operation by u1 succeeds
    And [s1, s2] are mounted for u1

  Scenario: Bad and good authorization
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [bad token, token]
    Then last operation by u1 fails
    And last operation by u2 succeeds
    And [s1, s2] are mounted for u2

   Scenario: Bad authorization
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [bad token, bad token]
    Then last operation by u1 fails
    And last operation by u2 fails