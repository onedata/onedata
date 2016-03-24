Feature: Multiprovider_proxy_replication

  Background:
    Given environment is defined in multiprovider_proxy_env.json
    And storage directories are empty
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]

  Scenario: Create files and see them on external provider
    When u1 creates regular files [file1, file2, file3] on client1
    Then u2 waits 5 seconds on client2 # wait for events handling
    And u2 sees [file1, file2, file3] in /spaces/s1 on client2
    And u2 sees [file1, file2, file3] in . on client2

  Scenario: Create empty file and read it on external provider
    When u1 creates regular files [file] on client1
    Then u2 waits 5 seconds on client2 # wait for events handling
    And u2 reads "" from file on client2