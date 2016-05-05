Feature: LUMA provider test

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token
    And oneclient is started for u1

  Scenario: Operations on POSIX storage
    When u1 creates regular files [spaces/posix/file1]
    And u1 writes "TEST TEXT ONEDATA POSIX" to spaces/posix/file1
    Then u1 reads "TEST TEXT ONEDATA POSIX" from spaces/posix/file1

  Scenario: Operations on CEPH storage
    When u1 creates regular files [spaces/ceph/file1]
    And u1 writes "TEST TEXT ONEDATA CEPH" to spaces/ceph/file1
    Then u1 reads "TEST TEXT ONEDATA CEPH" from spaces/ceph/file1

  Scenario: Operations on Amazon S3 storage
    When u1 creates regular files [spaces/s3/file1]
    And u1 writes "TEST TEXT ONEDATA S3" to spaces/s3/file1
    Then u1 reads "TEST TEXT ONEDATA S3" from spaces/s3/file1