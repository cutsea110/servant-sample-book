using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

namespace ServantClientBook
{
    #region Address
    [JsonObject("Address")]
    public class Address
    {
        [JsonProperty("addressId")]
        public int? addressId { get; set; }
        [JsonProperty("postcode")]
        public string postcode { get; set; }
        [JsonProperty("prefecture")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Prefecture prefecture { get; set; }
        [JsonProperty("address")]
        public string address { get; set; }
        [JsonProperty("building")]
        public string building { get; set; }
        [JsonProperty("tel")]
        public string tel { get; set; }
        [JsonProperty("fax")]
        public string fax { get; set; }
        [JsonProperty("email")]
        public string email { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Author
    [JsonObject("Author")]
    public class Author
    {
        [JsonProperty("authorId")]
        public int? authorId { get; set; }
        [JsonProperty("name")]
        public string name { get; set; }
        [JsonProperty("gender")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender gender { get; set; }
        [JsonProperty("birth")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime birth { get; set; }
        [JsonProperty("age")]
        public int age { get; set; }
        [JsonProperty("address")]
        public Address address { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Publisher
    [JsonObject("Publisher")]
    public class Publisher
    {
        [JsonProperty("publisherId")]
        public int? publisherId { get; set; }
        [JsonProperty("name")]
        public string name { get; set; }
        [JsonProperty("companyType")]
        [JsonConverter(typeof(StringEnumConverter))]
        public CompanyType companyType { get; set; }
        [JsonProperty("address")]
        public Address address { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Book
    [JsonObject("Book")]
    public class Book
    {
        [JsonProperty("bookId")]
        public int? bookId { get; set; }
        [JsonProperty("title")]
        public string title { get; set; }
        [JsonProperty("isbn")]
        public string isbn { get; set; }
        [JsonProperty("category")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Category category { get; set; }
        [JsonProperty("description")]
        public string description { get; set; }
        [JsonProperty("publishedBy")]
        public Publisher publishedBy { get; set; }
        [JsonProperty("authors")]
        public List<Author> authors { get; set; }
        [JsonProperty("publishedAt")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime publishedAt { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
}
