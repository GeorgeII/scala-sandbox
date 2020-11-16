package com.github.georgeii

import java.time.LocalDateTime

case class WeekInfo(
  weekNumber: Int,
  headline: String,
  description: String,
  storiesUrls: Iterable[String],
  timestamp: LocalDateTime
)
