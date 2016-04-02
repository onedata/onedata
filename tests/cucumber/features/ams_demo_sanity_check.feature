Feature: Ams_demo_sanity_check

  Background:
    Given environment is defined in ams_demo_env.json
    And storage directories are empty
    And environment is up
    And [u1, u2, u1, u2, u1, u2] start oneclients [client1_p1, client2_p1, client1_p2, client2_p2, client1_p3, client2_p3] in
      [/home/u1/onedata, /home/u2/onedata, /home/u1/onedata, /home/u2/onedata, /home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1_p1, client_host_2_p1, client_host_1_p2, client_host_2_p2, client_host_1_p3, client_host_2_p3] respectively,
      using [token, token, token, token, token, token]

#
# Operations on shared space, locally accessible on each provider, changes are replicated and rtransfer is used
#

  Scenario: Local/Local space - create files and see them on external provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file1, spaces/s1/file2, spaces/s1/file3] on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 sees [file1, file2, file3] in spaces/s1 on client2_p2

  Scenario: Local/Local space - create empty file and read it on external provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 reads "" from spaces/s1/file on client2_p2

  Scenario: Local/Local space - write to file and check size on remote provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And size of u2's spaces/s1/file is 17 bytes on client2_p2

  Scenario: Local/Local space - write to file and read on remote provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file on client2_p2

  Scenario: Local/Local space - big file transfer with MD5 check
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes 16 MB of random characters to spaces/s1/file on client1_p1 and saves MD5
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 checks MD5 of spaces/s1/file on client2_p2

#
# Operations done on local space, and verified on unsupported space through provider_proxy
#

  Scenario: Local/Proxy space - create files and see them on external provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file1, spaces/s1/file2, spaces/s1/file3] on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 sees [file1, file2, file3] in spaces/s1 on client2_p3

  Scenario: Local/Proxy space - create empty file and read it on external provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 reads "" from spaces/s1/file on client2_p3

  Scenario: Local/Proxy space - write to file and check size on remote provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And size of u2's spaces/s1/file is 17 bytes on client2_p3

  Scenario: Local/Proxy space - write to file and read on remote provider
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p1
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file on client2_p3

  Scenario: Local/Proxy space - big file transfer with MD5 check
    When u1 waits 5 seconds on client1_p1
    And u1 creates regular files [spaces/s1/file] on client1_p1
    And u1 writes 16 MB of random characters to spaces/s1/file on client1_p1 and saves MD5
    Then u1 waits 10 seconds on client1_p1 # wait for events handling
    And u2 checks MD5 of spaces/s1/file on client2_p3

#
# Operations done on unsupported space, and verified on local space
#

  Scenario: Proxy/Local space - create files and see them on external provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file1, spaces/s1/file2, spaces/s1/file3] on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 sees [file1, file2, file3] in spaces/s1 on client2_p1

  Scenario: Proxy/Local space - create empty file and read it on external provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 reads "" from spaces/s1/file on client2_p1

  Scenario: Proxy/Local space - write to file and check size on remote provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And size of u2's spaces/s1/file is 17 bytes on client2_p1

  Scenario: Proxy/Local space - write to file and read on remote provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file on client2_p1

  Scenario: Proxy/Local space - big file transfer with MD5 check
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes 16 MB of random characters to spaces/s1/file on client1_p3 and saves MD5
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 checks MD5 of spaces/s1/file on client2_p1

#
# Operations done on unsupported space, and verified on unsupported space
#

  Scenario: Proxy/Proxy space - create files and see them on external provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file1, spaces/s1/file2, spaces/s1/file3] on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 sees [file1, file2, file3] in spaces/s1 on client2_p3

  Scenario: Proxy/Proxy space - create empty file and read it on external provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 reads "" from spaces/s1/file on client2_p3

  Scenario: Proxy/Proxy space - write to file and check size on remote provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And size of u2's spaces/s1/file is 17 bytes on client2_p3

  Scenario: Proxy/Proxy space - write to file and read on remote provider
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file on client1_p3
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file on client2_p3

  Scenario: Proxy/Proxy space - big file transfer with MD5 check
    When u1 waits 5 seconds on client1_p3
    And u1 creates regular files [spaces/s1/file] on client1_p3
    And u1 writes 16 MB of random characters to spaces/s1/file on client1_p3 and saves MD5
    Then u1 waits 10 seconds on client1_p3 # wait for events handling
    And u2 checks MD5 of spaces/s1/file on client2_p3

  Scenario: List spaces dir
    When u1 waits 5 seconds on client1_p1
    Then u1 sees [s1, s2, u1_space, u2_space] in spaces on client1_p1
    And u2 sees [s1, s2, u1_space, u2_space] in spaces on client2_p1
    And u1 sees [s1, s2, u1_space, u2_space] in spaces on client1_p2
    And u2 sees [s1, s2, u1_space, u2_space] in spaces on client2_p2
    And u1 sees [s1, s2, u1_space, u2_space] in spaces on client1_p3
    And u2 sees [s1, s2, u1_space, u2_space] in spaces on client2_p3