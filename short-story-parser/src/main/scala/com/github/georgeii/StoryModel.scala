package com.github.georgeii

import java.time.LocalDateTime

case class StoryModel(
  name: String,
  author: String,
  categories: Iterable[String],
  body: String,
  likesNumber:Int,
  commentsNumber: Int,
  timestamp: LocalDateTime
)
